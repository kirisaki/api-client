{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Header
  (
    -- * Field of HTTP header
    Fields
  , fromList
  , toList
  , toList'

    -- * Header name
  , FieldName
  , fieldName
  , unFieldName

    -- * Header value
  , FieldValue
  , fieldValue
  , unFieldValue
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encoding  (text)
import           Data.Attoparsec.Text as A
import qualified Data.ByteString      as BSS
import           Data.CaseInsensitive (CI, mk, original)
import           Data.Char
import           Data.Either
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import qualified Data.List            as L
import           Data.Text            as T
import           Data.Text.Encoding

-- | Collection of HTTP header fields.
--   Duplicated header fields are allowed in <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>,
--   but this makes implements complecated, so treat field as unique in this module.
type Fields = HM.HashMap FieldName FieldValue

-- | Construct header fields with the supplied mappings.
--   It returns `Left` value when tries to build a field with the first pair which includes invalid key or name , or both
fromList :: [(T.Text, T.Text)] ->  Either Text Fields
fromList kvs =
  HM.fromList <$> forM kvs (
  \(k, v) ->
    case (fieldName k, fieldValue v) of
      (Left _, Left _)     -> Left "invalid field name and value"
      (Left _, Right _)    -> Left "invalid field name"
      (Right _, Left _)    -> Left "invalid field value"
      (Right k', Right v') -> Right (k', v')
  )

-- | Return a list of fields.
toList :: Fields -> [(FieldName, FieldValue)]
toList = HM.toList

-- | Return a list of 'ByteString'-encoded fields.
toList' :: Fields -> [(CI BSS.ByteString, BSS.ByteString)]
toList' = L.map (\(k, v) -> (unFieldName k, unFieldValue v)) . toList

-- | A field name of a HTTP header.
newtype FieldName = FieldName
  { unFieldName :: CI BSS.ByteString -- ^ Unwrap field name.
  } deriving(Show, Eq, Ord)

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
      Left e  -> fail $ T.unpack e

instance FromJSONKey FieldName where
  fromJSONKey = FromJSONKeyTextParser f
    where
      f = either (fail . T.unpack) return . fieldName

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>.
fieldName :: Text -> Either Text FieldName
fieldName t =
  let
    p = A.takeWhile1 (\c -> isAscii c && isAlphaNum c || elem c ("!#$%&'*+-.^_`|~" :: String))
  in
    case feed (parse p t) "" of
      Done "" n -> Right . FieldName . mk $ encodeUtf8 n
      Done _ _  -> Left "included invalid a character character"
      Fail {}   -> Left "included invalid a character character"
      Partial _ -> Left "lack input"

-- | A field value of a HTTP header.
newtype FieldValue = FieldValue
  { unFieldValue :: BSS.ByteString -- ^ Unwrap field value.
  } deriving(Show, Eq, Ord)

instance ToJSON FieldValue where
  toJSON = String . decodeUtf8 . unFieldValue

instance FromJSON FieldValue where
  parseJSON = withText "FieldValue" $
    \t -> case fieldValue t of
      Right n -> return n
      Left e  -> fail $ T.unpack e

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>.
fieldValue :: Text -> Either Text FieldValue
fieldValue t =
  let
    p = A.takeWhile1 (\c -> isAscii c && isPrint c)
  in
    case feed (parse p t) "" of
      Done "" n -> Right . FieldValue $ encodeUtf8 n
      Done _ _  -> Left "included invalid a character character"
      Fail {}   -> Left "included invalid a character character"
      Partial _ -> Left "lack input"
