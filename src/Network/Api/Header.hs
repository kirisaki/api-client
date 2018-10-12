{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Header
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Header
  (
    -- * Field of HTTP header
    Header
  , toHeader
  , toHeaderUtf8
  , fromHeader
  , fromHeaderUtf8

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
import           Data.Aeson.Encoding        (text)
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BSS
import           Data.CaseInsensitive       (CI, mk, original)
import           Data.Char                  (ord)
import           Data.Either
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Word8

-- | Collection of HTTP header fields.
--   Duplicated header fields are allowed in <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>,
--   but this makes implements complecated, so treat field as unique in this module.
type Header = HM.HashMap FieldName FieldValue

-- | Construct header fields with the supplied mappings.
--   It returns `Left` value when tries to build a field with the first pair which includes invalid key or name , or both
toHeader :: [(BSS.ByteString, BSS.ByteString)] ->  Either Text Header
toHeader kvs =
  HM.fromList <$> forM kvs (
  \(k, v) ->
    case (fieldName k, fieldValue v) of
      (Left _, Left _)     -> Left "invalid field name and value"
      (Left _, Right _)    -> Left "invalid field name"
      (Right _, Left _)    -> Left "invalid field value"
      (Right k', Right v') -> Right (k', v')
  )

-- | A version of 'toHeader' taking utf-8 text.
toHeaderUtf8 :: [(T.Text, T.Text)] -> Either Text Header
toHeaderUtf8 = toHeader . L.map (\(k, v) -> (encodeUtf8 k, encodeUtf8 v))

-- | Return a list of 'ByteString' encoded fields.
fromHeader :: Header -> [(CI BSS.ByteString, BSS.ByteString)]
fromHeader = L.map (\(k, v) -> (unFieldName k, unFieldValue v)) . HM.toList

-- | A version of 'fromHeader' returning utf-8 text
fromHeaderUtf8 :: Header ->  [(T.Text, T.Text)]
fromHeaderUtf8 = L.map (\(k, v) -> (decodeUtf8 $ original k, decodeUtf8 v)) . fromHeader

-- | A field name of a HTTP header.
newtype FieldName = FieldName
  { unFieldName :: CI BSS.ByteString -- ^ Unwrap field name.
  } deriving(Show, Eq, Ord)

instance Hashable FieldName where
  hashWithSalt i = hashWithSalt i . unFieldName

instance ToJSON FieldName where
  toJSON = String . decodeUtf8 . original . unFieldName

instance ToJSONKey FieldName where
  toJSONKey = ToJSONKeyText f g
    where
      f = decodeUtf8 . original . unFieldName
      g = text . f

instance FromJSON FieldName where
  parseJSON = withText "FieldName" $
    \t -> case fieldName $ encodeUtf8 t of
      Right n -> return n
      Left e  -> fail $ T.unpack e

instance FromJSONKey FieldName where
  fromJSONKey = FromJSONKeyTextParser f
    where
      f = either (fail . T.unpack) return . fieldName . encodeUtf8

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>.
fieldName :: BSS.ByteString -> Either Text FieldName
fieldName t =
  let
    p = A.takeWhile1
      (\c ->
         isAscii c &&
         isAlphaNum c ||
         elem c (L.map (fromIntegral . ord) "!#$%&'*+-.^_`|~")
      )
  in
    case feed (parse p t) "" of
      Done "" n -> Right . FieldName $ mk n
      Done _ _  -> Left "included invalid a character(Done)"
      Fail {}   -> Left "included invalid a character(Fail)"
      Partial _ -> Left "lack input"

-- | A field value of a HTTP header.
newtype FieldValue = FieldValue
  { unFieldValue :: BSS.ByteString -- ^ Unwrap field value.
  } deriving(Show, Eq, Ord)

instance ToJSON FieldValue where
  toJSON = String . decodeUtf8 . unFieldValue

instance FromJSON FieldValue where
  parseJSON = withText "FieldValue" $
    \t -> case fieldValue $ encodeUtf8 t of
      Right n -> return n
      Left e  -> fail $ T.unpack e

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>.
fieldValue :: BSS.ByteString -> Either Text FieldValue
fieldValue t =
  let
    p = BSS.cons <$> A.satisfy isPrint <*> A.takeWhile isValue
    isValue c =
      isAscii c &&
      ( isPrint c ||
      c == _tab ||
      c == _space )
  in
    case feed (parse p t) "" of
      Done "" n -> (Right . FieldValue) n
      Done _ _  -> Left "included invalid a character character"
      Fail {}   -> Left "included invalid a character character"
      Partial _ -> Left "lack input"
