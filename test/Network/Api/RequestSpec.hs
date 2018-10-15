{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Api.RequestSpec where

import           Network.Api.Header
import           Network.Api.Query
import           Network.Api.Request
import           Network.Api.Service
import           Network.Api.Url
import           Test.Hspec
import           TestUtils

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.CaseInsensitive     (mk)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List                as L
import           Data.Proxy
import qualified Data.Text                as T
import qualified Network.HTTP.Client      as C
import qualified Network.HTTP.Types       as HT
import           Network.Wai.Handler.Warp (run)
import           Servant                  hiding (GET, POST, toHeader)
import           Servant.Server           (serve)

type Api =
  "user" :> Capture "id" Integer :> Get '[JSON] Value :<|>
  "user" :> Capture "id" Integer :> "comment" :> Capture "article" Integer :> ReqBody '[JSON] Value :> PostNoContent '[JSON] () :<|>
  "token" :> ReqBody '[JSON] Value :> Post '[JSON] Value :<|>
  "hoge" :> Get '[JSON] Value :<|>
  EmptyAPI

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

sampleService :: Service
sampleService = Service
                { baseUrl = "https://example.net"
                , methods =
                  [ Method GET "user/:id"
                  , Method POST "user/{id}/comment/{article}"
                  , Method POST "token"
                  ]
                , defaultHeader =
                    right $ toHeader [("User-Agent", "Netscape Navigator")]
                , tokenHeaderName = Just . right $ fieldName "Authorization"
                , tokenHeaderPrefix = Just "Bearer"
                , tokenQueryName = Nothing
                }

defReq :: Request
defReq = Request
         { reqMethod = GET
         , reqPath = ""
         , reqParams = []
         , reqQuery = HM.empty
         , reqHeader = HM.empty
         , reqBody = ""
         , reqToken = Nothing
         , reqAltUrl = Nothing
         }

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
  describe "attachToken" specAttachToken
  describe "injectUrl"  specInjectUrl
  describe "lookupMethod" specLookupMethod
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
  let service = sampleService
        { baseUrl = "http://localhost:"
          `T.append` (T.pack . show) mockServerPort
        }
  it "normal case" $ \(man, _) -> do
    let req = defReq
          { reqMethod = GET
          , reqPath = "user/:id"
          , reqParams = [("id", "1234")]
          }
    res <- call man req service
    (decode $ resBody res :: Maybe Value)
      `shouldBe`
      decode "{\"id\":\"1234\",\"name\":\"nyaan\"}"

specAttachToken :: Spec
specAttachToken = do
  it "Request has no token and service doesn't requires it."
    pending
  it "Request has no token, but service requiires it."
    pending
  it "Request has token and service requires it."
    pending
  it "Request has token, but service doesn't requires it."
    pending

specInjectUrl :: Spec
specInjectUrl = do
  it "path with colon parameters 1" $
    inject "/user/:id" [("id", "1234")] `shouldBe` Right "/user/1234"
  it "path with colon parameters 2" $
    inject "/user/:id/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
  it "path with braced parameters" $
    inject "/user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
  it "parameter between raw paths" $
    inject "/user/{id}/comment" [("id", "42")] `shouldBe` Right "/user/42/comment"
  it "parameter between raw paths with trailing slash" $
    inject "/user/{id}/comment/" [("id", "42")] `shouldBe` Right "/user/42/comment"
  it "path without head slash" $
    inject "user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
  it "parameter in the head without slash" $
    inject "{id}/" [("id", "42")] `shouldBe` Right "/42"
  it "mixed parameters" $
    inject "/user/{id}/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
  it "extra parameters" $
    inject "/user/{id}" [("id", "1234"), ("nyaan", "hoge")] `shouldBe` Right "/user/1234"
  it "lack parameters" $
    inject "/user/:id/comment/:num" [("id", "42")] `shouldBe` Left "lack parameters"

specLookupMethod :: Spec
specLookupMethod = do
  it "colon path" $
    lookupMethod
    ( defReq
      { reqMethod = GET
      , reqPath = "user/:id"
      }) sampleService
    `shouldBe`
    Just (Method GET "user/:id")
  it "braced path" $
    lookupMethod
    ( defReq
      { reqMethod = POST
      , reqPath = "user/{id}/comment/{article}"
      , reqParams = [("id", "1234"), ("article", "331")]
      }) sampleService
    `shouldBe`
    Just (Method POST "user/{id}/comment/{article}")
  it "head slash different" $
    lookupMethod
    ( defReq
      { reqMethod = POST
      , reqPath = "/user/{id}/comment/{article}"
      , reqParams = [("id", "1234"), ("article", "331")]
      }) sampleService
    `shouldBe`
    Just (Method POST "user/{id}/comment/{article}")
  it "parameter in path" $
    lookupMethod
    ( defReq
      { reqMethod = GET
      , reqPath = "user/1234"
      }) sampleService
    `shouldBe`
    Just (Method GET "user/:id")
  it "mixed path" $
    lookupMethod
    ( defReq
      { reqMethod = POST
      , reqPath = "user/{id}/comment/:article"
      }) sampleService
    `shouldBe`
    Just (Method POST "user/{id}/comment/{article}")
  it "not exist" $
    lookupMethod
    ( defReq
      { reqMethod = GET
      , reqPath = "nyaan/:aaa"
      }) sampleService
    `shouldBe`
    Nothing
  it "invalid method" $
    lookupMethod
    ( defReq
      { reqMethod = GET
      , reqPath = "user/{id}/comment/{article}"
      , reqParams = [("id", "1234"), ("article", "331")]
      }) sampleService
    `shouldBe`
    Nothing

specBuildHttpRequest :: Spec
specBuildHttpRequest = do
  context "cases which succeed" $ do
    it "normal case without token" $ do
      req <- C.parseUrlThrow "https://example.net/user/1234"
      let expected =
            req { C.requestHeaders =
                    [ (mk "User-Agent", "Netscape Navigator")]
                }
      actual <- buildHttpRequest
        ( defReq
          { reqMethod = GET
          , reqPath = "user/:id"
          , reqParams = [("id", "1234")]
          }
        ) sampleService
      actual `shouldBe` expected
    it "token at header" $ do
      req <- C.parseUrlThrow "https://example.net/user/1234"
      let expected =
            req { C.requestHeaders =
                    [ (mk "User-Agent", "Netscape Navigator")
                    , (mk "Authorization", "Bearer fuga")
                    ]
                }
      actual <- buildHttpRequest
        ( defReq
          { reqMethod = GET
          , reqPath = "user/:id"
          , reqParams = [("id", "1234")]
          , reqToken = Just $ Token "fuga" Nothing
          }
        ) sampleService
      actual `shouldBe` expected
    it "token at query string" $ do
      req <- C.parseUrlThrow "https://example.net/user/1234"
      let service = sampleService
            { tokenHeaderName = Nothing
            , tokenHeaderPrefix = Nothing
            , tokenQueryName = Just "token"
            }
      let expected =
            C.setQueryString
            [("token", Just "fuga")]
            req { C.requestHeaders =
                    [(mk "User-Agent", "Netscape Navigator")]
                }
      actual <- buildHttpRequest
        ( defReq
          { reqMethod = GET
          , reqPath = "user/:id"
          , reqParams = [("id", "1234")]
          , reqToken = Just $ Token "fuga" Nothing
          }
        ) service
      actual `shouldBe` expected
    it "has additional queries" $ do
      req <- C.parseUrlThrow "https://example.net/user/1234?a=aaa&b"
      let expected = req
            { C.requestHeaders =
                [ (mk "User-Agent", "Netscape Navigator")] }
      actual <- buildHttpRequest
        ( defReq
          { reqMethod = GET
          , reqPath = "user/:id"
          , reqParams = [("id", "1234")]
          , reqQuery = toQuery [("a", Just "aaa"), ("b", Nothing)]
          }
        ) sampleService
      actual `shouldBe` expected
    it "has additional headers" $ do
      req <- C.parseUrlThrow "https://example.net/user/1234"
      let expected = req
            { C.requestHeaders =
                [ (mk "X-Nyaan", "nyaan")
                , (mk "Accept", "application/nyaan.v3+json")
                , (mk "User-Agent", "Netscape Navigator")] }
      actual <- buildHttpRequest
        ( defReq
          { reqMethod = GET
          , reqPath = "user/:id"
          , reqParams = [("id", "1234")]
          , reqHeader = right $ toHeader
            [ ("x-nyaan", "nyaan")
            , ("Accept", "application/nyaan.v3+json")
            ]
          , reqToken = Just $ Token "fuga" Nothing
          }) sampleService
      actual `shouldBe` expected

  context "cases which fail" $ do
    it "method not defined" $
      let
        isMethodNotDefined MethodNotDefined = True
        isMethodNotDefined _                = False
      in
        buildHttpRequest
        ( defReq
          { reqMethod = GET
          , reqPath = "nyaan"
          }
        ) sampleService `shouldThrow` isMethodNotDefined
    it "failed to injecr url params" $
      let
        isFailedToInject (FailedToInjectUrlParams _) = True
        isFailedToInject _                           = False
      in
        buildHttpRequest
        ( defReq
          { reqMethod =  GET
          , reqPath = "user/:id"
          }
        ) sampleService `shouldThrow` isFailedToInject
