{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Network.ApiSpec where

import Test.Hspec

import Control.Concurrent (forkIO, killThread)
import Control.Exception.Safe
import Data.Aeson
import Data.Proxy
import Network.Api
import qualified Network.HTTP.Client     as C
import Network.Wai.Handler.Warp (run)
import Servant hiding(GET, POST)
import Servant.Server (serve)

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

mockServer :: IO ()
mockServer = run 8080 (serve (Proxy @ Api) server)

runMockServer :: IO () -> IO ()
runMockServer action = do
  tid <- forkIO mockServer
  action `finally` killThread tid


sampleService :: Service
sampleService = Service
                "https://example.net"
                [ Method GET "user/:id"
                , Method POST "user/{id}/comment/{article}"
                , Method POST "token"
                ]
                [("User-Agent", "nyaan")]
                (Just "Authorization")
                (Just "Bearer")
                Nothing

instance Eq C.Request where
  x == y = and 
           [ eq C.host
           , eq C.port
           , eq C.secure
           , eq C.requestHeaders
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
spec = around_ runMockServer $ do
  describe "injectUrl" $ do
    it "path with colon parameters 1" $
      injectUrlParams "/user/:id" [("id", "1234")] `shouldBe` Right "/user/1234"
    it "path with colon parameters 2" $
      injectUrlParams "/user/:id/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "path with braced parameters" $
      injectUrlParams "/user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "parameter between raw paths" $
      injectUrlParams "/user/{id}/comment" [("id", "42")] `shouldBe` Right "/user/42/comment"
    it "parameter between raw paths with trailing slash" $
      injectUrlParams "/user/{id}/comment/" [("id", "42")] `shouldBe` Right "/user/42/comment"
    it "path without head slash" $
      injectUrlParams "user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "parameter in the head without slash" $
      injectUrlParams "{id}/" [("id", "42")] `shouldBe` Right "/42"
    it "mixed parameters" $
        injectUrlParams "/user/{id}/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "extra parameters" $
      injectUrlParams "/user/{id}" [("id", "1234"), ("nyaan", "hoge")] `shouldBe` Right "/user/1234"
    it "lack parameters" $
      injectUrlParams "/user/:id/comment/:num" [("id", "42")] `shouldBe` Left "lack parameters"

  describe "lookupMethod" $ do
    it "colon path" $
      lookupMethod ( Request
                     GET "user/:id"
                     [("id", "1234")]
                     [] [] "" Nothing Nothing
                   ) sampleService `shouldBe` Just (Method GET "user/:id") 
    it "braced path" $
      lookupMethod ( Request
                     POST "user/{id}/comment/{article}"
                     [("id", "1234"), ("article", "331")]
                     [] [] "" Nothing Nothing
                   ) sampleService `shouldBe` Just (Method POST "user/{id}/comment/{article}")
    it "head slash different" $
      lookupMethod ( Request
                     POST "/user/{id}/comment/{article}"
                     [("id", "1234"), ("article", "331")]
                     [] [] "" Nothing Nothing
                   ) sampleService `shouldBe` Just (Method POST "user/{id}/comment/{article}")
    it "parameter in path" $
      lookupMethod ( Request
                     GET "user/1234"
                     []
                     [] [] "" Nothing Nothing
                   ) sampleService `shouldBe` Just (Method GET "user/:id") 
    it "mixed path" $
      lookupMethod ( Request
                     POST "user/{id}/comment/:article"
                     [("id", "1234"), ("article", "331")]
                     [] [] "" Nothing Nothing
                   ) sampleService `shouldBe` Just (Method POST "user/{id}/comment/{article}")
    it "not exist" $
      lookupMethod ( Request
                     GET "nyaan/:aaa"
                     []
                     [] [] "" Nothing Nothing
                   ) sampleService `shouldBe` Nothing
    it "invalid method" $
      lookupMethod ( Request
                     GET "user/{id}/comment/{article}"
                     [("id", "1234"), ("article", "331")]
                     [] []  "" Nothing Nothing
                   ) sampleService `shouldBe` Nothing

  describe "buildHttpRequest" $ do
    it "normal case" $ do
      expected <- C.parseUrlThrow "https://example.net/user/1234"
      actual <- buildHttpRequest ( Request
                                   GET "user/:id"
                                   [("id", "1234")]
                                   [] [] ""
                                   (Just $ Token "fuga" Indefinitely) Nothing
                                 ) sampleService
      actual `shouldBe` expected

