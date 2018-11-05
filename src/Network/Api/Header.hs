{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  , emptyHeader
  , toHeader
  , toHeaderUtf8
  , toHeaderWith
  , fromHeader
  , fromHeaderUtf8
  , fromHeaderWith

    -- * Header name
  , FieldName
  , fieldName
  , unFieldName

    -- * Header value
  , FieldValue
  , fieldValue
  , unFieldValue
  ) where

import           Control.Applicative        as AP
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encoding        (text)
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as SBS
import qualified Data.CaseInsensitive       as CI
import           Data.Char                  (ord)
import           Data.Either
import qualified Data.Foldable              as FO
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Data.List                  as L
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word8
import qualified Dhall                      as DH
import qualified Dhall.Core                 as DHC

-- | Collection of HTTP header fields.
--   Duplicated header fields are allowed in <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>,
--   but this makes implements complecated, so treat field as unique in this module.
type Header = HM.HashMap FieldName FieldValue

instance DH.Interpret Header where
  autoWith _ = HM.fromList <$>
               DH.list (DH.pair fieldNameDH fieldValueDH)

-- | Empty Header.
emptyHeader :: Header
emptyHeader = HM.empty

-- | Construct header fields with the supplied mappings.
--   It returns `Left` value when tries to build a field with the first pair which includes invalid key or name , or both
toHeader :: [(CI.CI SBS.ByteString, SBS.ByteString)] ->  Either Text Header
toHeader = toHeaderWith id

-- | Utf-8 version of 'toHeader'.
toHeaderUtf8 :: [(CI.CI T.Text, T.Text)] -> Either Text Header
toHeaderUtf8 = toHeaderWith encodeUtf8

-- | To 'Header' with mapping function.
toHeaderWith :: CI.FoldCase a => (a -> SBS.ByteString) -> [(CI.CI a, a)] -> Either Text Header
toHeaderWith f kvs = HM.fromList <$> forM kvs (
  \(k, v) ->
    case ((fieldName . CI.map f) k, fieldValue $ f v) of
      (Left _, Left _)     -> Left "invalid field name and value"
      (Left _, Right _)    -> Left "invalid field name"
      (Right _, Left _)    -> Left "invalid field value"
      (Right k', Right v') -> Right (k', v')
  )

-- | Return a list of 'ByteString' encoded fields.
fromHeader :: Header -> [(CI.CI SBS.ByteString, SBS.ByteString)]
fromHeader = fromHeaderWith id

-- | Utf-8 version of 'fromHeader'
fromHeaderUtf8 :: Header ->  [(CI.CI T.Text, T.Text)]
fromHeaderUtf8 = fromHeaderWith $ decodeUtf8With ignore

-- | From 'Header' with mapping function.
fromHeaderWith :: CI.FoldCase a => (SBS.ByteString -> a) -> Header -> [(CI.CI a, a)]
fromHeaderWith f = L.map (
  \(k, v) ->
    ( (CI.map f . unFieldName) k
    , (f . unFieldValue) v
    )
  ) . HM.toList

-- | A field name of a HTTP header.
newtype FieldName = FieldName
  { unFieldName :: CI.CI SBS.ByteString -- ^ Unwrap field name.
  } deriving(Show, Eq, Ord)

instance Hashable FieldName where
  hashWithSalt i = hashWithSalt i . unFieldName

instance ToJSON FieldName where
  toJSON = String . decodeUtf8With ignore . CI.original . unFieldName

instance ToJSONKey FieldName where
  toJSONKey = ToJSONKeyText f g
    where
      f = decodeUtf8With ignore . CI.original . unFieldName
      g = text . f

instance FromJSON FieldName where
  parseJSON = withText "FieldName" $
    \t -> case (fieldName . CI.mk . encodeUtf8) t of
      Right n -> return n
      Left e  -> fail $ T.unpack e

instance FromJSONKey FieldName where
  fromJSONKey = FromJSONKeyTextParser f
    where
      f = either (fail . T.unpack) return . fieldName . CI.mk . encodeUtf8

instance DH.Interpret FieldName where
  autoWith _ = fieldNameDH

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>.
fieldName :: CI.CI SBS.ByteString -> Either Text FieldName
fieldName t =
  let
    p = A.takeWhile1
      (\c ->
         isAscii c &&
         isAlphaNum c ||
         elem c (L.map (fromIntegral . ord) "!#$%&'*+-.^_`|~")
      )
  in
    case feed (parse p (CI.original t)) "" of
      Done "" n -> (Right . FieldName . CI.mk) n
      Done _ _  -> Left "included invalid a character(Done)"
      Fail {}   -> Left "included invalid a character(Fail)"
      Partial _ -> Left "lack input"

fieldNameDH :: DH.Type FieldName
fieldNameDH = DH.Type {..}
  where
    extract (DHC.TextLit (DHC.Chunks [] t)) =
      (either (const Nothing) Just . fieldName . CI.mk . encodeUtf8) t
    extract  _                      = AP.empty
    expected = DHC.Text

-- | A field value of a HTTP header.
newtype FieldValue = FieldValue
  { unFieldValue :: SBS.ByteString -- ^ Unwrap field value.
  } deriving(Show, Eq, Ord)

instance ToJSON FieldValue where
  toJSON = String . decodeUtf8With ignore . unFieldValue

instance FromJSON FieldValue where
  parseJSON = withText "FieldValue" $
    \t -> case fieldValue $ encodeUtf8 t of
      Right n -> return n
      Left e  -> fail $ T.unpack e

instance DH.Interpret FieldValue where
  autoWith _ = fieldValueDH

-- | Make field name. Refer <https://tools.ietf.org/html/rfc7230#section-3.2 RFC7230>.
fieldValue :: SBS.ByteString -> Either Text FieldValue
fieldValue t =
  let
    p = SBS.cons <$> A.satisfy isPrint <*> A.takeWhile isValue
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

fieldValueDH :: DH.Type FieldValue
fieldValueDH = DH.Type {..}
  where
    extract (DHC.TextLit (DHC.Chunks [] t)) =
      (either (const Nothing) Just . fieldValue . encodeUtf8) t
    extract  _                      = AP.empty
    expected = DHC.Text

