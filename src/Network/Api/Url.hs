{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
    Url
  , Port
  , fromPort
  , toPort
    -- * URL query parameter
  , Query
  , toQuery
  , toQueryBS
  , toQuery'
  , toQueryBS'
  , toQueryWith
  , fromQuery
  , fromQueryBS
  , fromQueryWith
    -- * Path
  , UrlPath
  , fromUrlPath
  , toUrlPath
    -- * URL encoded string
  , UrlEncoded
  , urlEncode
  , urlEncodeBS
  , urlDecode
  , urlDecodeBS

    -- * Utilities
  , inject
  ) where

import           Network.Api.Parser

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Encoding      (text)
import           Data.Attoparsec.Text     as A
import qualified Data.ByteString          as SBS
import           Data.Char
import           Data.Hashable
import           Data.HashMap.Strict      as HM
import qualified Data.List                as L
import           Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word
import qualified Network.HTTP.Types.URI   as U

-- | Convert Url to 'ByteString'
fromUrl :: Url -> SBS.ByteString
fromUrl (Url s a h p ps) = undefined

-- | Wrapped URL.
data Url = Url
  { scheme  :: Scheme
  , auth    :: Maybe Auth
  , host    :: Host
  , port    :: Maybe Port
  , urlPath :: UrlPath
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

-- | A pair of user and password.
newtype Auth = Auth (UrlEncoded, UrlEncoded) deriving (Show, Ord, Eq)

-- | Wrapped port number. Port number is limited from 0 to 65535.
--   See <https://tools.ietf.org/html/rfc793#section-3.1 RFC793>.
newtype Port = Port
  { fromPort :: Int
  } deriving (Show, Ord, Eq)

toPort :: Int -> Either T.Text Port
toPort n =
  if 0 <= n && n <= 65535
  then (Right . Port) n
  else Left "Invalid port number"

-- | Wrapped hostname.
--   It can't deal with Punycode hostname.
newtype Host = Host
  { unHost :: SBS.ByteString
  } deriving (Show, Ord, Eq)

fromHost :: Host -> T.Text
fromHost = decodeUtf8With ignore . unHost

toHost :: T.Text -> Either T.Text Host
toHost t =
  case feed (parse hostname t) "" of
    Done "" h -> Right h
    _         -> Left "Failed to parse host."

hostname :: Parser Host
hostname =
  let
    label = T.append <$> takeWhile1 isAlphaNum' <*>
      option ""
      ( T.append <$>
        A.takeWhile (\c -> isAlphaNum' c || c == '-' ) <*>
        takeWhile1 isAlphaNum'
      )
    isAlphaNum' c = isAscii c && isAlphaNum c
  in
    Host . encodeUtf8 . T.intercalate "." <$> label `sepBy` char '.'


-- | Wrapped path.
newtype UrlPath = UrlPath { unUrlPath :: [UrlEncoded] } deriving (Show, Eq, Ord)

fromUrlPath :: UrlPath -> T.Text
fromUrlPath = T.intercalate "/" . L.map urlDecode . unUrlPath

toUrlPath :: T.Text -> UrlPath
toUrlPath = UrlPath . L.map urlEncode . L.filter (\p -> p /= "" && p /= ".." && p /= ".") . T.splitOn "/"

(</>) :: UrlPath -> UrlPath -> UrlPath
x </> y = UrlPath $ unUrlPath x ++ unUrlPath y


-- | URL with parameters.
data Piece = Raw UrlEncoded | Param T.Text


-- | Collection of URL query parameters.
--   Behaviour when duplicated query keys at URL is not defined,
--   but it makes implements complecated, so treat keys as unique in this module.
type Query = HM.HashMap UrlEncoded (Maybe UrlEncoded)

-- | Construct 'Query' with the supplied mappings.
toQueryBS :: [(SBS.ByteString, Maybe SBS.ByteString)] -> Query
toQueryBS = toQueryWith id

-- | Utf-8 version of 'toQuery'
toQuery :: [(T.Text, Maybe T.Text)] -> Query
toQuery = toQueryWith encodeUtf8

-- | Construct 'Query' without parameter-less field.
toQueryBS' :: [(SBS.ByteString, SBS.ByteString)] -> Query
toQueryBS' = toQueryWith' id

-- | Utf-8 version of 'toQuery\''
toQuery' :: [(T.Text, T.Text)] -> Query
toQuery' = toQueryWith' encodeUtf8

-- | To 'Query' with mapping functions.
toQueryWith :: (a -> SBS.ByteString) -> [(a, Maybe a)] -> Query
toQueryWith f = HM.fromList . L.map (\(k, v) -> (urlEncodeBS $ f k, urlEncodeBS . f <$> v))

-- | To 'Query' with mapping functions without parameter-less field.
toQueryWith' :: (a -> SBS.ByteString) -> [(a, a)] -> Query
toQueryWith' f = HM.fromList . L.map (\(k, v) -> (urlEncodeBS $ f k, (Just . urlEncodeBS . f) v))

-- | Return a list of 'ByteString' encoded fields.
fromQueryBS :: Query -> [(SBS.ByteString, Maybe SBS.ByteString)]
fromQueryBS = fromQueryWith id

-- | Utf-8 version 'fromQuery'.
fromQuery :: Query -> [(T.Text, Maybe T.Text)]
fromQuery = fromQueryWith (decodeUtf8With ignore)

-- | From 'Query' with mapping functions.
fromQueryWith :: (SBS.ByteString -> a)  -> Query -> [(a, Maybe a)]
fromQueryWith f = L.map (\(k, v) -> (f $ urlDecodeBS k, f . urlDecodeBS <$> v)) . HM.toList

-- | URIEncoded 'ByteString'.
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: SBS.ByteString -- ^ Unwrap encoded string.
  } deriving (Eq, Show, Ord)

instance Hashable UrlEncoded where
  hashWithSalt i = hashWithSalt i . unUrlEncoded

instance ToJSON UrlEncoded where
  toJSON = String . urlDecode

instance ToJSONKey UrlEncoded where
  toJSONKey = ToJSONKeyText f g
    where
      f = urlDecode
      g = text . f

instance FromJSON UrlEncoded where
  parseJSON = withText "UrlEncoded" (return . urlEncode)

instance FromJSONKey UrlEncoded where
  fromJSONKey = FromJSONKeyTextParser (return . urlEncode)

-- | ByteString text to URI encoded bytestring.
--   It converts ' '(0x20) to '+'
urlEncodeBS :: SBS.ByteString -> UrlEncoded
urlEncodeBS = urlEncodeWith id

-- | Utf-8 version of 'urlEncode'.
urlEncode :: T.Text -> UrlEncoded
urlEncode = urlEncodeWith encodeUtf8

-- | Encode with a encoder.
urlEncodeWith :: (a -> SBS.ByteString) -> a -> UrlEncoded
urlEncodeWith enc = UrlEncoded . U.urlEncode True . enc

-- | Decode URI encoded 'Builder' to strict 'ByteString'.
urlDecodeBS :: UrlEncoded -> SBS.ByteString
urlDecodeBS = urlDecodeWith id

-- | Utf-8 version of 'urlDecode'.
urlDecode :: UrlEncoded -> T.Text
urlDecode = urlDecodeWith $ decodeUtf8With ignore

-- | Decode with a decoder.
urlDecodeWith :: (SBS.ByteString -> a) -> UrlEncoded -> a
urlDecodeWith dec (UrlEncoded t) = (dec . U.urlDecode True) t

-- | Inject parameters to a path represented with colon or braces.
inject :: Text -> [(Text, Text)] -> Either Text Text
inject path params = undefined
