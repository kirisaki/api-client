{-# LANGUAGE OverloadedStrings #-}
module Network.Api.UrlSpec where

import           Network.Api.Url
import           Test.Hspec
import           TestUtils

spec :: Spec
spec =
  describe "inject" specInject

specInject :: Spec
specInject = do
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
