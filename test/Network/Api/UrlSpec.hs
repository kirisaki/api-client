{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Api.UrlSpec where

import           Network.Api.Internal
import           Network.Api.Url
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
import           TestUtils

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString                as SBS
import           Data.Char
import           Data.Functor
import qualified Data.HashMap.Strict            as HM
import qualified Data.List                      as L
import           Data.Maybe
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Word

spec :: Spec
spec = do
  prop "parseUrl/buildUrl" $
    \url ->
      urlText url `shouldBe` (buildUrl . right . parseUrl . urlText) url
  prop "fromAuthority/toAuthority" $
    \auth ->
      authorityText auth `shouldBe` (fromAuthority . right . toAuthority . authorityText) auth
  prop "fromHost/toHost" $
    \h ->
      hostText h `shouldBe` (fromHost . right . toHost . hostText) h
  prop "fromUrlPath/toUrlPath" $
    \path ->
      ( T.cons '/' .
        T.intercalate "/" .
        L.filter notRelative  .
        T.splitOn "/"
      ) (pathText path)
      ==
      (fromUrlPath . right . toUrlPath) (pathText path)
  prop " urlEncode/urlDecode" $
    \t -> t == (urlDecode . urlEncode) t
  prop "toJSON/fromJSON for Query" $
    \kvs -> kvs ==
    (fromQuery .
     success . fromJSON . toJSON . toQuery') kvs
  prop "toQuery/fromQuery" $
    \kvs -> kvs ==
    (fromQuery . toQuery') kvs

arbitraryUnreserved :: Gen Char
arbitraryUnreserved = arbitrary `suchThat`
    (\c -> isAlphaNum c ||
           c == '-' ||
           c == '.' ||
           c == '_' ||
           c == '~')

arbitrarySubDelims :: Gen Char
arbitrarySubDelims = elements ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

arbitraryPctEncoded :: Gen T.Text
arbitraryPctEncoded =
  let
      pct = elements "%"
      hex = elements "0123456789ABCDEF"
    in
      (\a b c -> T.pack [a, b, c]) <$> pct <*> hex <*> hex

arbitraryUserinfoText :: Gen T.Text
arbitraryUserinfoText = T.concat <$> listOf
  ( oneof
    [ T.singleton <$> arbitraryUnreserved
    , T.singleton <$> arbitrarySubDelims
    , T.singleton <$> elements ":"
    , arbitraryPctEncoded
    ]
  )

arbitraryHostText :: Gen T.Text
arbitraryHostText =
  let
    just1 = L.replicate 1
    alphaNum = arbitrary `suchThat` (\c -> isAlphaNum c && isAscii c)
    alphaNumHyp = arbitrary `suchThat` (\c -> (isAlphaNum c || c == '-') && isAscii c)
    single = just1 <$> alphaNum
    multi = L.concat <$> sequence [just1 <$> alphaNum, listOf alphaNumHyp, just1 <$> alphaNum]
    segment = oneof [single, multi]
  in
    T.pack . L.intercalate "." <$> listOf1 segment

newtype HostText = HostText
  { hostText :: T.Text } deriving (Show, Eq, Ord)

instance Arbitrary HostText where
  arbitrary = HostText <$> arbitraryHostText

arbitraryPortText :: Gen T.Text
arbitraryPortText = T.pack . show <$> (arbitrary :: Gen Word16)

newtype AuthorityText = AuthorityText
  { authorityText :: T.Text } deriving (Show, Eq, Ord)

instance Arbitrary AuthorityText where
  arbitrary =
    let
      ut = Just <$> (arbitraryUserinfoText <&> (`T.snoc` '@'))
      ht = arbitraryHostText
      pt = Just . T.cons ':' <$> arbitraryPortText
    in
      (\p a s -> AuthorityText . fromJust $ p <> Just a <> s) <$> ut <*> ht <*> pt

newtype UrlText = UrlText
  { urlText :: T.Text } deriving (Show, Eq, Ord)

instance Arbitrary UrlText where
  arbitrary =
    let
      st = Just <$> elements ["", "http://", "https://"]
      at = authorityText <$> arbitrary
      pt = Just . ("/" <>) . pathText <$> arbitrary
    in
      (\p a s -> UrlText . fromJust $ p <> Just a <> s) <$> st <*> at <*> pt
