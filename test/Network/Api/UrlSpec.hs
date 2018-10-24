{-# LANGUAGE OverloadedStrings #-}
module Network.Api.UrlSpec where

import           Network.Api.Url
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
import           TestUtils

import           Data.Aeson
import qualified Data.ByteString                as SBS
import           Data.Char
import qualified Data.HashMap.Strict            as HM
import qualified Data.List                      as L
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Data.Text.Encoding

spec :: Spec
spec = do
  prop " urlEncode/urlDecode" $
    \t -> t == (urlDecode . urlEncode) t
  prop "fromUrlPath/toUrlPath" $
    \path ->
      ( T.intercalate "/" .
        L.filter notRelative  .
        T.splitOn "/"
      ) path
      ==
      (fromUrlPath . toUrlPath) path
  prop "toPathParams/fromPathParams" $
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
        (normalize . fromPathParams . right . toPathParams . pathText) p
  describe "inject" specInject
  describe "Query function props" specQuery

notRelative :: (IsString a, Eq a) => a -> Bool
notRelative p = p /= "." && p /= ".." && p /= ""

newtype PathText = PathText { pathText :: T.Text } deriving (Show, Ord, Eq)
instance Arbitrary PathText where
  arbitrary =
    let
      pathChar = arbitrary
        `suchThat` (\c ->
                      c /= '{' &&
                      c /= '}' &&
                      c /= '/' &&
                      c /= ':')
      paramText = listOf1 pathChar `suchThat` notRelative
      colonParam = (:) ':' <$> paramText
      bracedParam = (\s -> "{" ++ s ++ "}") <$> paramText
      segment = oneof [colonParam, bracedParam, paramText]
    in
      PathText . T.intercalate "/" . L.map T.pack  <$> listOf segment
  shrink (PathText "") = []
  shrink (PathText path) = ( shrink .
                             PathText .
                             T.drop 1 .
                             T.dropWhile (/= '/')
                           ) path

specQuery :: Spec
specQuery = do
  prop "toJSON/fromJSON for Query" $
    \kvs -> HM.fromList kvs ==
    (HM.fromList . L.sort . fromQuery .
     success . fromJSON . toJSON . toQuery) kvs
  prop "toQuery/fromQuery" $
    \kvs -> HM.fromList kvs ==
    (HM.fromList . L.sort . fromQuery . toQuery) kvs

specInject :: Spec
specInject = do
  let inject' p a = toPathParams p >>= flip inject a
  it "path with colon parameters 1" $
    inject' "/user/:id" [("id", "1234")] `shouldBe` (Right . toUrlPath) "/user/1234"
  it "path with colon parameters 2" $
    inject' "/user/:id/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` (Right . toUrlPath) "/user/42/comment/3"
  it "path with braced parameters" $
    inject' "/user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` (Right . toUrlPath) "/user/42/comment/3"
  it "parameter between raw paths" $
    inject' "/user/{id}/comment" [("id", "42")] `shouldBe` (Right . toUrlPath) "/user/42/comment"
  it "parameter between raw paths with trailing slash" $
    inject' "/user/{id}/comment/" [("id", "42")] `shouldBe` (Right . toUrlPath) "/user/42/comment"
  it "path without head slash" $
    inject' "user/{id}/comment/{num}" [("id", "42"), ("num", "3")] `shouldBe` (Right . toUrlPath) "/user/42/comment/3"
  it "parameter in the head without slash" $
    inject' "{id}/" [("id", "42")] `shouldBe` (Right . toUrlPath) "/42"
  it "mixed parameters" $
    inject' "/user/{id}/comment/:num" [("id", "42"), ("num", "3")] `shouldBe` (Right . toUrlPath) "/user/42/comment/3"
  it "extra parameters" $
    inject' "/user/{id}" [("id", "1234"), ("nyaan", "hoge")] `shouldBe` (Right . toUrlPath) "/user/1234"
  it "lack parameters" $
    inject' "/user/:id/comment/:num" [("id", "42")] `shouldBe` Left "Lacks following parameters: num"
