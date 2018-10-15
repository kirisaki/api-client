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
import qualified Data.ByteString          as SBS
import           Data.Hashable
import           Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word
import qualified Network.HTTP.Types.URI   as U

-- | Convert Url to 'ByteString'
fromUrl :: Url -> SBS.ByteString
fromUrl (Url s a h p) = undefined

-- | Wrapped URL.
data Url = Url
  { scheme   :: Scheme
  , auth     :: Maybe Auth
  , hostName :: HostName
  , urlPath  :: UrlPath
  } deriving (Show, Ord, Eq)

-- | URL scheme.
data Scheme
  = Http
  | Https
  deriving (Show, Ord, Eq, Read)

fromScheme :: Scheme -> SBS.ByteString
fromScheme Http  = "http"
fromScheme Https = "https"

toScheme :: SBS.ByteString -> Maybe Scheme
toScheme "http"  = Just Http
toScheme "https" = Just Https
toScheme _       = Nothing

newtype Auth = Auth
  { unAuth :: (SBS.ByteString, SBS.ByteString)
  } deriving (Show, Ord, Eq)

-- | Wrapped port number
newtype Port = Port
  { unPort :: Word16
  } deriving (Show, Ord, Eq)

toPort :: Integer -> Maybe Port
toPort n =
  if 0 <= n && n <= 65535
  then (Just . Port . fromIntegral) n
  else Nothing

-- | Wrapped hostname.
newtype HostName = HostName
  { unHost :: SBS.ByteString
  } deriving (Show, Ord, Eq)

-- | Wrapped path.
newtype UrlPath = UrlPath
  { unUrlPath :: SBS.ByteString
  } deriving (Show, Ord, Eq)

-- | URIEncoded 'ByteString'.
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: SBS.ByteString -- ^ Unwrap encoded string.
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
urlEncode :: SBS.ByteString -> UrlEncoded
urlEncode = UrlEncoded . U.urlEncode True

-- | Utf-8 version of 'urlEncode'.
urlEncodeUtf8 :: T.Text -> UrlEncoded
urlEncodeUtf8 = urlEncode . encodeUtf8

-- | Decode URI encoded bytestrings to ByteString.
urlDecode :: UrlEncoded -> SBS.ByteString
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
