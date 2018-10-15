{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Url
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Url
  (
     -- * URL encoded string
    UrlEncoded
  , urlEncode
  , urlEncodeUtf8
  , urlDecode
  , urlDecodeUtf8

    -- * Utilities
  , inject
  ) where

import           Network.Api.Parser

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Encoding      (text)
import           Data.Attoparsec.Text
import qualified Data.ByteString          as BSS
import           Data.Hashable
import           Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word
import qualified Network.HTTP.Types.URI   as U

-- | Wrapped URL.
newtype Url = Url
  { unUrl :: BSS.ByteString
  } deriving (Show, Ord, Eq)

-- | Wrapped URL scheme.
newtype Scheme = Scheme
  { unScheme :: BSS.ByteString
  } deriving (Show, Ord, Eq)

-- | Wrapped port number
newtype Port = Port
  { unPort :: Word16
  }

-- | Wrapped hostname.
newtype Host = Host
  { unHost :: BSS.ByteString
  } deriving (Show, Ord, Eq)

-- | Wrapped path.
newtype Path = Path
  { unPath :: BSS.ByteString
  } deriving (Show, Ord, Eq)

-- | URIEncoded 'ByteString'.
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: BSS.ByteString -- ^ Unwrap encoded string.
  } deriving (Eq, Show, Ord)

instance Hashable UrlEncoded where
  hashWithSalt i = hashWithSalt i . unUrlEncoded

instance ToJSON UrlEncoded where
  toJSON = String . urlDecodeUtf8

instance ToJSONKey UrlEncoded where
  toJSONKey = ToJSONKeyText f g
    where
      f = urlDecodeUtf8
      g = text . f

instance FromJSON UrlEncoded where
  parseJSON = withText "UrlEncoded" (return . urlEncodeUtf8)

instance FromJSONKey UrlEncoded where
  fromJSONKey = FromJSONKeyTextParser (return . urlEncodeUtf8)

-- | ByteString text to URI encoded bytestring.
--   It converts '='
urlEncode :: BSS.ByteString -> UrlEncoded
urlEncode = UrlEncoded . U.urlEncode True

-- | Utf-8 version of 'urlEncode'.
urlEncodeUtf8 :: T.Text -> UrlEncoded
urlEncodeUtf8 = urlEncode . encodeUtf8

-- | Decode URI encoded bytestrings to ByteString.
urlDecode :: UrlEncoded -> BSS.ByteString
urlDecode = U.urlDecode True . unUrlEncoded

-- | Utf-8 version of 'urlDecode'.
urlDecodeUtf8 :: UrlEncoded -> T.Text
urlDecodeUtf8 = decodeUtf8With ignore . urlDecode

-- | Inject parameters to a path represented with colon or braces.
inject :: Text -> [(Text, Text)] -> Either Text Text
inject path params =
  let
    inject' (Right path, "") = Right path
    inject' (Left e, _) = Left e
    inject' (Right path, remain) =
      case feed (parse segment remain) "" of
        Done rem new ->
          case new of
            Param k ->
              case lookup k params of
                Just v ->
                  inject' (Right (path `snoc` '/' `append` v), rem)
                Nothing ->
                  Left "lack parameters"
            Raw "" ->
              inject' (Right path, rem)
            Raw t ->
              inject' (Right (path `snoc` '/' `append` t), rem)
        _ ->
          Left "failed parsing"
  in
    inject' (Right "", path)
