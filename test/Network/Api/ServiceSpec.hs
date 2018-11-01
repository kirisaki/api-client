{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Api.ServiceSpec where

import           Network.Api.Internal
import           Network.Api.Service
import           Network.Api.Url
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           TestUtils

import qualified Data.List             as L
import qualified Data.Text             as T

spec :: Spec
spec = do
  prop "parsePathParams/buildPathParams" $
    \p ->
      let
        normalize =
          T.intercalate "/" .
          L.map ( T.dropAround (\c -> c == '{' || c == '}') .
                  T.dropWhile (== ':')
                ) .
          L.filter notRelative .
          T.splitOn "/"
      in
        (normalize . pathText) p
        ==
        (normalize . buildPathParams . right . parsePathParams . pathText) p
  describe "inject" specInject

specInject :: Spec
specInject = do
  let inject' p a = parsePathParams p >>= flip inject a
  it "path with colon parameters 1" $
    inject' "/user/:id" [("id", "1234")] `shouldBe` parseUrlPath "/user/1234"
  it "path with colon parameters 2" $
    inject' "/user/:id/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` parseUrlPath "/user/42/comment/3"
  it "path with braced parameters" $
    inject' "/user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` parseUrlPath "/user/42/comment/3"
  it "parameter between raw paths" $
    inject' "/user/{id}/comment" [("id", "42")] `shouldBe` parseUrlPath "/user/42/comment"
  it "parameter between raw paths with trailing slash" $
    inject' "/user/{id}/comment/" [("id", "42")] `shouldBe` parseUrlPath "/user/42/comment"
  it "path without head slash" $
    inject' "user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` parseUrlPath "/user/42/comment/3"
  it "parameter in the head without slash" $
    inject' "{id}/" [("id", "42")] `shouldBe` parseUrlPath "/42"
  it "mixed parameters" $
    inject' "/user/{id}/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` parseUrlPath "/user/42/comment/3"
  it "extra parameters" $
    inject' "/user/{id}" [("id", "1234"), ("nyaan", "hoge")] `shouldBe` parseUrlPath "/user/1234"
  it "lack parameters" $
    inject' "/user/:id/comment/:num" [("id", "42")] `shouldBe` Left "Lacks following parameters: num"
