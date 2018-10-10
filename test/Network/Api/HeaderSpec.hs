{-# LANGUAGE OverloadedStrings #-}

module Network.Api.HeaderSpec where

import           Test.Hspec
import           Network.Api.Header

import           Data.Aeson
import           Data.CaseInsensitive
import           Data.Either
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

-- Misc for aeson
isError :: Result a -> Bool
isError (Error _) = True
isError _ = False

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

spec :: Spec
spec = do
  describe "fieldName" specFieldName
  describe "fieldValue" specFieldValue
  describe "field" specField

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

  context "use Data.Aeson.toJSON" $ do
    it "normal case" $
      (encode $ HM.fromList [(fromRight undefined $ fieldName "hogehoge", "fuga" :: Text)]) `shouldBe` "{\"hogehoge\":\"fuga\"}"

  context "use Data.Aeson.fromJSON" $ do
    it "normal case" $
      pending

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

specField :: Spec
specField = do
  it "normal case" $
    field "Accept" "application/someservice+json" `shouldSatisfy` isRight
  it "invalid field name" $
    field "" "hoge" `shouldBe` Left "invalid field name"
  it "invalid field content" $
    field "User-Agent" "" `shouldBe` Left "invalid field value"
  it "both are invalid" $
    field "spam/egg" "あああ" `shouldBe` Left "invalid field name and value"
