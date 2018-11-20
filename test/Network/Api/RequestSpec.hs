{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Api.RequestSpec where

import           Network.Api.Header
import           Network.Api.Request
import           Network.Api.Service
import           Network.Api.Url
import           Test.Hspec
import           TestUtils

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.CaseInsensitive       (mk)
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Data.Proxy
import qualified Data.Text                  as T
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Types         as HT
import qualified Network.HTTP.Types.Version as V
import           Network.Wai.Handler.Warp   (run)
import           Servant                    hiding (GET, HttpVersion (..), POST,
                                             toHeader)
import           Servant.Server             (serve)

type Api =
  "api" :>
  ( "user" :> Capture "id" Integer :> Get '[JSON] Value :<|>
    "user" :> Capture "id" Integer :> "comment" :> Capture "article" Integer :> ReqBody '[JSON] Value :> PostNoContent '[JSON] () :<|>
    "token" :> ReqBody '[JSON] Value :> Post '[JSON] Value :<|>
    "hoge" :> Get '[JSON] Value :<|>
    EmptyAPI
  )

server :: Server Api
server = getUser :<|> postComment :<|> getToken :<|> hoge :<|> emptyServer
  where
    getUser uid = return $ object ["id" .= show uid, "name" .= String "nyaan"]
    postComment _ _ _ = return ()
    getToken _ = return $ object ["token" .= String "opensesami", "expire_in" .= String "3600"]
    hoge = return $ object ["foo" .= String "bar"]

mockServerPort :: Int
mockServerPort = 9432

mockServer :: IO ()
mockServer = run mockServerPort (serve (Proxy @ Api) server)

instance Eq C.Request where
  x == y = and
           [ eq C.host
           , eq C.port
           , eq C.secure
           , eq (L.sort . C.requestHeaders)
           , eq C.path
           , eq C.queryString
           , eq C.method
           , eq C.proxy
           , eq C.redirectCount
           , eq C.responseTimeout
           , eq C.requestVersion
           ]
    where
      eq f = f x == f y

spec :: Spec
spec = do
  describe "call" specCall
  describe "lookupEndpoint" specLookupEndpoint
  describe "buildHttpRequest" specBuildHttpRequest


withMock :: ((C.Manager, ThreadId) -> IO ()) -> IO ()
withMock = bracket (
  do
    tid <- forkIO mockServer
    man <- newManager defaultManagerSettings
    return (man, tid)
  )
  (killThread . snd)

specCall :: Spec
specCall = around withMock $ do
  let defReq = Request
        { reqMethod = GET
        , reqPath = PathParams []
        , reqParams = []
        , reqQuery = emptyQuery
        , reqHeader = emptyHeader
        , reqBody = ""
        , reqToken = Nothing
        , reqAltUrl = Nothing
        , reqAltHttpVersion = Nothing
        }
  let sampleService = Service
        { baseUrl = right . parseUrl $ "http://localhost:" <> (T.pack . show) mockServerPort <> "/api"
        , endpoints =
            [ Endpoint GET  (PathParams [Raw "user", Param "id"])
            , Endpoint POST (PathParams [Raw "user", Param "id", Raw "comment", Param "article"])
            , Endpoint POST (PathParams [Raw "token"])
            ]
        , httpVersion = HttpVersion 1 1
        , defaultHeader =
            right $ toHeader [("User-Agent", "Netscape Navigator")]
        , tokenHeaderName = Just . right $ fieldName "Authorization"
        , tokenHeaderPrefix = Just "Bearer"
        , tokenQueryName = Nothing
        }
  it "normal case" $ \(man, _) -> do
    let req = defReq
          { reqMethod = GET
          , reqPath = PathParams [Raw "user", Param "id"]
          , reqParams = [("id", "1234")]
          }
    res <- call man req sampleService
    (decode $ resBody res :: Maybe Value)
      `shouldBe`
      decode "{\"id\":\"1234\",\"name\":\"nyaan\"}"

specBuildHttpRequest :: Spec
specBuildHttpRequest = do
  let defReq = Request
        { reqMethod = GET
        , reqPath = PathParams []
        , reqParams = []
        , reqQuery = emptyQuery
        , reqHeader = emptyHeader
        , reqBody = ""
        , reqToken = Nothing
        , reqAltUrl = Nothing
        , reqAltHttpVersion = Nothing
        }
  let defHttpReq = C.defaultRequest
        { C.host = "example.net"
        , C.port = 443
        , C.secure = True
        , C.requestHeaders  = [(mk "User-Agent", "Netscape Navigator")]
        , C.path            = ""
        , C.queryString     = ""
        , C.method          = "GET"
        , C.requestBody     = C.RequestBodyBS ""
        , C.requestVersion  = V.HttpVersion 2 0
        }
  let sampleService = Service
        { baseUrl = right $ parseUrl "https://example.net/api"
        , endpoints =
            [ Endpoint GET  (PathParams [Raw "user", Param "id"])
            , Endpoint POST (PathParams [Raw "user", Param "id", Raw "comment", Param "article"])
            , Endpoint POST (PathParams [Raw "token"])
            ]
        , httpVersion = HttpVersion 2 0
        , defaultHeader =
            right $ toHeader [("User-Agent", "Netscape Navigator")]
        , tokenHeaderName = Just . right $ fieldName "Authorization"
        , tokenHeaderPrefix = Just "Bearer"
        , tokenQueryName = Nothing
        }

  it "A simple case" $
    buildHttpRequest defReq { reqPath = PathParams [Raw "token"], reqMethod = POST } sampleService
    `shouldBe`
    Right defHttpReq { C.path = "api/token", C.method = "POST" }

  it "With arg and token" $
    buildHttpRequest defReq { reqPath = PathParams [Raw "user", Param "id"]
                            , reqParams = [("id", "1234")]
                            , reqToken = Just $ Token "nyaan" Nothing } sampleService
    `shouldBe`
    Right defHttpReq { C.path = "api/user/1234"
               , C.requestHeaders  = [ (mk "User-Agent", "Netscape Navigator")
                                     , (mk "Authorization", "Bearer nyaan")
                                     ]
               }

  it "When a service requires the token at query." $
    buildHttpRequest defReq { reqPath = PathParams [Raw "user", Param "id", Raw "comment", Param "article"]
                            , reqParams = [("id", "1234"), ("article", "5")]
                            , reqToken = Just $ Token "nyaan" Nothing
                            , reqMethod = POST
                            }
    sampleService { tokenHeaderName = Nothing
                  , tokenHeaderPrefix = Nothing
                  , tokenQueryName = Just "token"
                  }
    `shouldBe`
    Right defHttpReq { C.path = "api/user/1234/comment/5"
               , C.queryString = "token=nyaan"
               , C.method = "POST"
               }

  it "Alternative URL." $
    buildHttpRequest defReq { reqPath = PathParams [Raw "token"]
                            , reqMethod = POST
                            , reqAltUrl = (Just . right . parseUrl) "http://localhost:8080/mock/api"
                            , reqAltHttpVersion = Just $ HttpVersion 1 1} sampleService
    `shouldBe`
    Right defHttpReq { C.host = "localhost"
               , C.port = 8080
               , C.secure = False
               , C.path = "mock/api/token"
               , C.method = "POST"
               , C.requestVersion = V.HttpVersion 1 1}

specLookupEndpoint :: Spec
specLookupEndpoint = do
  let defReq = Request
        { reqMethod = GET
        , reqPath = PathParams []
        , reqParams = []
        , reqQuery = emptyQuery
        , reqHeader = emptyHeader
        , reqBody = ""
        , reqToken = Nothing
        , reqAltUrl = Nothing
        , reqAltHttpVersion = Nothing
        }
  let sampleService = Service
        { baseUrl = right $ parseUrl "https://example.net/api"
        , endpoints =
            [ Endpoint GET  (PathParams [Raw "user", Param "id"])
            , Endpoint POST (PathParams [Raw "user", Param "id", Raw "comment", Param "article"])
            , Endpoint POST (PathParams [Raw "token"])
            ]
        , httpVersion = HttpVersion 2 0
        , defaultHeader =
            right $ toHeader [("User-Agent", "Netscape Navigator")]
        , tokenHeaderName = Just . right $ fieldName "Authorization"
        , tokenHeaderPrefix = Just "Bearer"
        , tokenQueryName = Nothing
        }
  let lookupEndpoint' m p = lookupEndpoint (defReq { reqMethod = m, reqPath = PathParams p }) sampleService
  let method m p = Right (Endpoint m (PathParams p))
  it "Just a URL." $
    lookupEndpoint' POST [Raw "token"] `shouldBe` method POST [Raw "token"]
  it "With a simple parameter." $
    lookupEndpoint' GET [Raw "user", Param "id"] `shouldBe` method GET [Raw "user", Param "id"]
  it "Overwrite a parameter." $
    lookupEndpoint' GET [Raw "user", Raw "123"] `shouldBe` method GET [Raw "user", Raw "123"]
  it "Wrong HTTP method." $
    lookupEndpoint' POST [Raw "user", Param "id"] `shouldBe` Left "Endpoint not found."
  it "'user' should be Raw." $
    lookupEndpoint' GET [Param "user", Param "id"] `shouldBe` Left "Endpoint not found."
  it "Not found." $
    lookupEndpoint' GET [Raw "nyaan"] `shouldBe` Left "Endpoint not found."

