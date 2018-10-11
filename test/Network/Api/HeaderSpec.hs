{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Api.HeaderSpec where

import           Network.Api.Header
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TestUtils

import           Data.Aeson            as AE
import qualified Data.ByteString       as BSS
import           Data.CaseInsensitive
import           Data.Char             (ord)
import           Data.Either
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Data.Word8


spec :: Spec
spec = do
  describe "Converting header" specConvertHeader
  describe "fieldName" specFieldName
  describe "fieldValue" specFieldValue

specConvertHeader :: Spec
specConvertHeader =
    prop "to/from JSON" $
      \(h :: Header) -> Just h == (decode . encode) h

specToHeader :: Spec
specToHeader = do
  it "normal case" $
    toHeader
    [ ("Accept", "application/someservice+json")
    , ("user-agent", "Netscape Communicator")
    ] `shouldBe`
    (Right $ HM.fromList
    [ (right $ fieldName "accept", right $ fieldValue "application/someservice+json")
    , (right $ fieldName "User-Agent", right $ fieldValue "Netscape Communicator")
    ])
  it "invalid field name" $
    toHeader
    [ ("Accept", "application/someservice+json")
    , ("user/agent", "Netscape Communicator")
    ] `shouldBe` Left "invalid field name"
  it "invalid field value" $
    toHeader
    [ ("Accept", "なんらか")
    , ("user-agent", "Netscape Communicator")
    ] `shouldBe` Left "invalid field value"
  it "both are invalid sametime" $
    toHeader
    [ ("every/thing", "間違っている")
    , ("user-agent", "Netscape Communicator")
    ] `shouldBe` Left "invalid field name and value"

specFromHeader :: Spec
specFromHeader =
  it "normal case" $ do
  let kvs =
        [ ("accept", "application/someservice+json")
        , ("User-Agent", "Netscape Communicator")
        ]
  let kvs' = L.map (\(k, v) -> (mk k, v)) kvs
  (L.sort . fromHeader . right $ toHeader kvs) `shouldBe` L.sort kvs'

specFieldName :: Spec
specFieldName = do
  prop "prop of FieldName" $
    \n -> Right n == (fieldName . original . unFieldName) n
  it "normal case1" $
    fieldName "Accept" `shouldSatisfy` isRight
  it "normal case2" $
    fieldName "User-Agent" `shouldSatisfy` isRight
  it "should be case-insensitive" $
    fieldName "Content-Length" `shouldBe` fieldName "content-length"
  it "include invalid character" $
    fieldName "something,wrong" `shouldSatisfy` isLeft
  it "empty text" $
    fieldName "" `shouldSatisfy` isLeft
  it "invalid characters in the end" $
    fieldName "ah?" `shouldSatisfy` isLeft
  it "include unprintable character" $
    fieldName "ah\taa" `shouldSatisfy` isLeft
  it "include non-ASCII character" $
    fieldName "にゃーん" `shouldSatisfy` isLeft

  it "encode key" $
    encode (HM.fromList [(right $ fieldName "hogehoge", "fuga" :: T.Text)]) `shouldBe` "{\"hogehoge\":\"fuga\"}"
  it "decode key" $
    (decode "{\"hogehoge\":\"fuga\"}" :: Maybe (HM.HashMap FieldName T.Text))
    `shouldBe` (Just $ HM.fromList [(right $ fieldName "hogehoge", "fuga")])

specFieldValue :: Spec
specFieldValue = do
  it "normal case" $
    fieldValue "Netscape" `shouldSatisfy` isRight
  it "include space" $
    fieldValue "Interbet Exprorer" `shouldSatisfy` isRight
  it "include symbols" $
    fieldValue "\"spam-sausage/egg*\"" `shouldSatisfy` isRight
  it "empty text" $
    fieldValue "" `shouldSatisfy` isLeft
  it "include non-ASCII character" $
    fieldValue "にゃーん……" `shouldSatisfy` isLeft

  context "use Data.Aeson.toJSON" $ do
    it "normal case" $
      toJSON <$> fieldValue "hogehoge" `shouldBe` (Right $ String "hogehoge")
    it "number should be converted to String" $
      toJSON <$> fieldValue "1234" `shouldBe` (Right $ String "1234")
    it "include symbol" $
      toJSON <$> fieldValue "ex-parrot" `shouldBe` (Right $ String "ex-parrot")

  context "use Data.Aeson.fromJSON" $ do
    it "normal case" $
      fromJSON "some text" `shouldBe` AE.Success (fromRight undefined $ fieldValue "some text")
    it "invalid value" $
      (fromJSON "あああ" :: AE.Result FieldValue) `shouldSatisfy` isError

-- For QuickCheck.
instance Arbitrary FieldName where
  arbitrary = fmap (right . fieldName) token
    where
      token = fmap BSS.pack (listOf1 tchar)
      tchar = arbitrary `suchThat`
        (\c -> isAscii c &&
               isAlphaNum c ||
               L.elem c (L.map (fromIntegral . ord) "!#$%&'*+-.^_`|~"))
  shrink = L.map (right . fieldName . BSS.pack) .
    L.filter (\s -> not (L.null s) && all isAlphaNum s) . shrink . BSS.unpack . original . unFieldName

instance Arbitrary FieldValue where
  arbitrary = fmap (right . fieldValue) fieldContent
    where
      fieldContent = fmap BSS.pack $ (:) <$> vchar <*> listOf vchar'
      vchar = arbitrary `suchThat` isPrint
      vchar' = arbitrary `suchThat`
        (\c -> isPrint c ||
               c == _tab ||
               c == _space
        )
  shrink = L.map (right . fieldValue . BSS.pack) .
    L.filter (not . L.null) . shrink . BSS.unpack . unFieldValue

instance Arbitrary Header where
  arbitrary = fmap HM.fromList (listOf ((,) <$> arbitrary <*> arbitrary))
  shrink = fmap HM.fromList . shrink . HM.toList

