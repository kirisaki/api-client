----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.Api.Header
  ( Field
  , field
  , getFieldName
  , getFieldValue
  , FieldName
  , fieldName
  , unFieldName
  , FieldValue
  , fieldValue
  , unFieldValue
  ) where

import           Data.Aeson
import           Data.Aeson.Encoding (text)
import           Data.Attoparsec.Text    as A
import qualified Data.ByteString         as BSS
import           Data.CaseInsensitive    (CI, mk, original)
import           Data.Char
import           Data.Hashable
import           Data.Text               as T
import           Data.Text.Encoding

-- | Field name and value of HTTP header.
data Field = Field { getFieldName :: FieldName
                               , getFieldValue :: FieldValue
                               } deriving (Show, Eq, Ord)

instance ToJSON Field where
  toJSON (Field (FieldName name) (FieldValue value)) = object [ (decodeUtf8 $ original name) .= (decodeUtf8 value) ]

-- | Make header field from 'Text'
field :: T.Text -> T.Text -> Either Text Field
field name value =
  case (fieldName name, fieldValue value) of
    (Left _, Left _) -> Left "invalid field name and value"
    (Left _, Right _) -> Left "invalid field name"
    (Right _, Left _) -> Left "invalid field value"
    (Right n, Right v) -> Right $ Field n v
    
-- | A field name of a HTTP header.
newtype FieldName = FieldName { unFieldName :: CI BSS.ByteString } deriving(Show, Eq, Ord)

instance Hashable FieldName where
  hashWithSalt i (FieldName n) = hashWithSalt i n

instance ToJSON FieldName where
  toJSON (FieldName n) = String . decodeUtf8 $ original n

instance ToJSONKey FieldName where
  toJSONKey = ToJSONKeyText f g
    where
      f (FieldName n) = decodeUtf8 $ original n
      g = text . f      

instance FromJSON FieldName where
  parseJSON = withText "FieldName" $
    \t -> case fieldName t of
      Right n -> return n
      Left e -> fail $ T.unpack e

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>
fieldName :: Text -> Either Text FieldName
fieldName t =
  let
    p = A.takeWhile1 (\c -> isAscii c && isAlphaNum c || elem c ("!#$%&'*+-.^_`|~" :: String))
  in
    case feed (parse p t) "" of
      Done "" n -> Right . FieldName . mk $ encodeUtf8 n
      Done _ _ -> Left "included invalid a character character"
      Fail _ _ _ -> Left "included invalid a character character"
      Partial _ -> Left "lack input"

-- | A field value of a HTTP header.
newtype FieldValue = FieldValue { unFieldValue :: BSS.ByteString } deriving(Show, Eq, Ord)

instance ToJSON FieldValue where
  toJSON = String . decodeUtf8 . unFieldValue

instance FromJSON FieldValue where
  parseJSON = withText "FieldValue" $
    \t -> case fieldValue t of
      Right n -> return n
      Left e -> fail $ T.unpack e

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>
fieldValue :: Text -> Either Text FieldValue
fieldValue t =
  let
    p = A.takeWhile1 (\c -> isAscii c && isPrint c)
  in
    case feed (parse p t) "" of
      Done "" n -> Right . FieldValue $ encodeUtf8 n
      Done _ _ -> Left "included invalid a character character"
      Fail _ _ _ -> Left "included invalid a character character"
      Partial _ -> Left "lack input"
