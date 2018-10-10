{-# LANGUAGE OverloadedStrings #-}

module Network.Api.HeaderSpec where

import           Network.Api.Header
import           Test.Hspec

import           Data.Aeson
import           Data.CaseInsensitive
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)

-- Misc for aeson
isError :: Result a -> Bool
isError (Error _) = True
isError _         = False

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

--Unsafe helper for tests
right :: Either a b -> b
right (Right b)  = b

left :: Either a b -> a
left (Left a) = a

spec :: Spec
spec = do
  describe "Fields and JSON can convert mutal" specConvertFields
  describe "fromList" specFromList
  describe "fieldName" specFieldName
  describe "fieldValue" specFieldValue

specConvertFields :: Spec
specConvertFields =
  let
    fields = HM.fromList
      [ (right $ fieldName "User-Agent", right $ fieldValue "Netscape Navigator")
      , (right $ fieldName "Accept", right $ fieldValue "application/json")
      ] :: Fields
    encoded = "{\"User-Agent\":\"Netscape Navigator\",\"Accept\":\"application/json\"}"
  in do
    it "JSON encode" $
      encode fields `shouldBe` encoded
    it "JSON decode" $
      decode encoded `shouldBe` Just fields

specFromList :: Spec
specFromList = do
  it "normal case" $
    fromList
    [ ("Accept", "application/someservice+json")
    , ("user-agent", "Netscape Communicator")
    ] `shouldBe`
    (Right $ HM.fromList
    [ (right $ fieldName "accept", right $ fieldValue "application/someservice+json")
    , (right $ fieldName "User-Agent", right $ fieldValue "Netscape Communicator")
    ])
  it "invalid field name" $
    fromList
    [ ("Accept", "application/someservice+json")
    , ("user/agent", "Netscape Communicator")
    ] `shouldBe` Left "invalid field name"
  it "invalid field value" $
    fromList
    [ ("Accept", "なんらか")
    , ("user-agent", "Netscape Communicator")
    ] `shouldBe` Left "invalid field value"
  it "both are invalid sametime" $
    fromList
    [ ("every/thing", "間違っている")
    , ("user-agent", "Netscape Communicator")
    ] `shouldBe` Left "invalid field name and value"

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
