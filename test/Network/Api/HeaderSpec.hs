{-# LANGUAGE OverloadedStrings #-}

module Network.Api.HeaderSpec where

import           Network.Api.Header
import           Test.Hspec
import           TestUtils

import           Data.Aeson
import           Data.ByteString
import           Data.CaseInsensitive
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import qualified Data.List            as L
import           Data.Text            (Text)
import           Data.Text.Encoding

spec :: Spec
spec = do
  describe "Fields and JSON can convert mutal" specConvertHeader
  describe "toHeader" specToHeader
  describe "fromHeader" specFromHeader
  describe "fieldName" specFieldName
  describe "fieldValue" specFieldValue

specConvertHeader :: Spec
specConvertHeader =
  let
    fields = HM.fromList
      [ (right $ fieldName "User-Agent", right $ fieldValue "Netscape Navigator")
      , (right $ fieldName "Accept", right $ fieldValue "application/json")
      ] :: Header
    encoded = "{\"User-Agent\":\"Netscape Navigator\",\"Accept\":\"application/json\"}"
  in do
    it "JSON encode" $
      encode fields `shouldBe` encoded
    it "JSON decode" $
      decode encoded `shouldBe` Just fields

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
  let kvs' = L.map (\(k, v) -> (mk $ encodeUtf8 k, encodeUtf8 v)) kvs
  (L.sort . fromHeader . right $ toHeader kvs) `shouldBe` L.sort kvs'

specFieldName :: Spec
specFieldName = do
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
    encode (HM.fromList [(right $ fieldName "hogehoge", "fuga" :: Text)]) `shouldBe` "{\"hogehoge\":\"fuga\"}"
  it "decode key" $
    (decode "{\"hogehoge\":\"fuga\"}" :: Maybe (HM.HashMap FieldName Text))
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
  it "include unprintable character" $
    fieldValue "ah\taa" `shouldSatisfy` isLeft
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
      fromJSON "some text" `shouldBe` Success (fromRight undefined $ fieldValue "some text")
    it "invalid value" $
      (fromJSON "あああ" :: Result FieldValue) `shouldSatisfy` isError
