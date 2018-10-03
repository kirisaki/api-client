{-# LANGUAGE OverloadedStrings #-}
module Network.ApiSpec where

import Test.Hspec
import Network.Api

spec :: Spec
spec = do
  describe "injectUrl" $ do
    it "path with colon parameters 1" $ do
      injectUrlParams "/user/:id" [("id", "1234")] `shouldBe` Right "/user/1234"
    it "path with colon parameters 2" $ do
      injectUrlParams "/user/:id/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "path with braced parameters" $ do
      injectUrlParams "/user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "mixed parameters" $ do
        injectUrlParams "/user/{id}/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` Right "/user/42/comment/3"
    it "extra parameters" $ do
      injectUrlParams "/user/{id}" [("id", "1234"), ("nyaan", "hoge")] `shouldBe` Right "/user/1234"
    it "lack parameters" $ do
      injectUrlParams "/user/:id/comment/:num" [("id", "42")] `shouldBe` Left "lack parameters"

  describe "lookupMethod" $ do
    let service
          = Service
            "https://example.net"
            Nothing
            Nothing
            Nothing
            [ Method GET "user/:id"
            , Method POST "user/{id}/comment/{article}"
            ]
            []
            Nothing
            Nothing
    it "colon path" $ do
      lookupMethod ( Request
                     GET
                     "user/:id"
                     [("id", "1234")]
                     []
                     []
                     ""
                   ) service `shouldBe` Just (Method GET "user/:id") 
    it "brace path" $ do
      lookupMethod ( Request
                     POST
                     "user/{id}/comment/{article}"
                     [("id", "1234"), ("article", "331")]
                     []
                     []
                     ""
                   ) service `shouldBe` Just (Method POST "user/{id}/comment/{article}")
    it "parameter in path" $ do
      lookupMethod ( Request
                     GET
                     "user/1234"
                     []
                     []
                     []
                     ""
                   ) service `shouldBe` Just (Method GET "user/:id") 
    it "mixed path" $ do
      lookupMethod ( Request
                     POST
                     "user/{id}/comment/:article"
                     [("id", "1234"), ("article", "331")]
                     []
                     []
                     ""
                   ) service `shouldBe` Just (Method POST "user/{id}/comment/{article}")
    it "not exist" $ do
      lookupMethod ( Request
                     GET
                     "nyaan/:aaa"
                     []
                     []
                     []
                     ""
                   ) service `shouldBe` Nothing
    it "invalid method" $ do
      lookupMethod ( Request
                     GET
                     "user/{id}/comment/{article}"
                     [("id", "1234"), ("article", "331")]
                     []
                     []
                     ""
                   ) service `shouldBe` Nothing
