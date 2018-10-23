{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Network.Api.UrlSpec where

import           Network.Api.Url
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TestUtils

import           Data.Aeson
import qualified Data.ByteString.Char8 as SBS
import           Data.Char
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Text             as T
import           Data.Text.Encoding

spec :: Spec
spec = do
  prop " urlEncode/urlDecode" $
    \t -> getASCIIString t == (SBS.unpack . urlDecode . urlEncode . SBS.pack . getASCIIString) t
  prop "toUrlPath/fromUrlPath" $
    \path ->
      (SBS.intercalate "/" .
       L.filter (\p -> p /= "" && p /= "..")  .
       SBS.split '/' .
       SBS.pack . getASCIIString ) path
      ==
      (fromPath . id @UrlPath . toPath .
       SBS.pack . getASCIIString) path
  describe "inject" $ it "" pending -- specInject
  describe "Query function props" specQuery

specQuery :: Spec
specQuery = do
  prop "toJSON/fromJSON for Query" $
    \kvs -> (HM.fromList . ascii) kvs ==
    (HM.fromList . L.sort . fromQuery .
     success . fromJSON . toJSON . toQuery . ascii) kvs
  prop "toQuery/fromQuery" $
    \kvs -> (HM.fromList . ascii) kvs ==
    (HM.fromList . L.sort . fromQuery . toQuery . ascii) kvs
  where
    ascii = L.sort . L.map (
      \(k, v) -> ( (encodeUtf8 . T.pack . getASCIIString) k
                 , fmap (encodeUtf8 . T.pack . getASCIIString) v
                 ) :: (SBS.ByteString, Maybe SBS.ByteString)
      )

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
