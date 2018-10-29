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
         , reqQuery = Nothing
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

