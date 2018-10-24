{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
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
    -- * URL
    Url
  , parseUrl
    -- * Authority
    -- ** Userinfo
  , Userinfo
  , fromUserinfo
  , toUserinfo
    -- ** Host
  , Host
  , fromHost
  , toHost
    -- ** Port
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
    -- * Path with parameters
  , PathParams
  , fromPathParams
  , toPathParams
    -- * URL encoded string
  , UrlEncoded
  , urlEncode
  , urlEncodeBS
  , urlDecode
  , urlDecodeBS

    -- * Utilities
  , inject
  ) where

import           Control.Applicative      as AP
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encoding      (text)
import           Data.Attoparsec.Text     as AT
import qualified Data.ByteString          as SBS
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.Hashable
import           Data.HashMap.Strict      as HM
import qualified Data.List                as L
import           Data.String              (IsString)
import           Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word
import           Dhall                    hiding (inject, string)
import           Dhall.Core
import qualified Network.HTTP.Types.URI   as U

-- | Parse Url
parseUrl :: T.Text -> Either Text Url
parseUrl = undefined

--url' :: Parse Url
--url' = Url <$>
--       scheme' <*>

-- | Wrapped URL.
data Url = Url
  { scheme    :: Scheme
  , authority :: Maybe Authority
  , urlPath   :: UrlPath
  } deriving (Show, Ord, Eq)

-- | URL scheme.
data Scheme
  = Http
  | Https
  deriving (Show, Ord, Eq, Read)

fromScheme :: Scheme -> T.Text
fromScheme Http  = "http"
fromScheme Https = "https"

toScheme :: T.Text -> Either T.Text Scheme
toScheme t =
  case parse' scheme' t of
    Done "" s -> Right s
    _         -> Left "Failed parsing scheme"

scheme' :: Parser Scheme
scheme' =
  string "http" $> Http <|>
  string "https" $> Https

-- | A pair of user and password.
data Authority = Authority
  { userinfo :: Maybe Userinfo
  , host     :: Host
  , port     :: Maybe Port
  } deriving (Show, Ord, Eq)

--authority' :: Parser Authority
--authority' = optional

-- | Wrapped userinfo
newtype Userinfo = Userinfo { unUserInfo :: UrlEncoded } deriving (Show, Eq, Ord)

fromUserinfo :: Userinfo -> T.Text
fromUserinfo = urlDecode . unUserInfo

toUserinfo :: T.Text -> Userinfo
toUserinfo = Userinfo . urlEncode

-- | Wrapped hostname.
--   It can't deal with IPv6 yet.
newtype Host = Host
  { unHost :: SBS.ByteString
  } deriving (Show, Ord, Eq)

fromHost :: Host -> T.Text
fromHost = decodeUtf8With ignore . unHost

toHost :: T.Text -> Either T.Text Host
toHost t =
  case parse' host' t of
    Done "" h -> Right h
    _         -> Left "Failed to parse host."

host' :: Parser Host
host' =
  let
    label = T.append <$> takeWhile1 isAlphaNum' <*>
      option ""
      ( T.append <$>
        AT.takeWhile (\c -> isAlphaNum' c || c == '-' ) <*>
        takeWhile1 isAlphaNum'
      )
    isAlphaNum' c = isAscii c && isAlphaNum c
  in
    Host . encodeUtf8 . T.intercalate "." <$> label `sepBy` char '.'

-- | Wrapped port number. Port number in URL is defined in
--   <https://tools.ietf.org/html/rfc3986#section-3.2.3 RFC3986> as
--  @port = *DIGIT@, but port numbers of protocols used
--  by HTTP (<https://tools.ietf.org/html/rfc793 TCP>
--  , <https://tools.ietf.org/html/rfc768 UDP>
--  , <https://tools.ietf.org/html/rfc4960#section-3.1 SCTP>) are
--  limited from 0 to 65535.
newtype Port = Port
  { unPort :: Word16
  } deriving (Show, Ord, Eq)

fromPort :: Port -> T.Text
fromPort = T.pack . show . unPort

toPort :: T.Text -> Either T.Text Port
toPort t =
  case parse' port' t of
    Done "" p -> Right p
    _         -> Left "Invalid port number"

port' :: Parser Port
port' = Port . fromIntegral <$> ((<= 65535) =|< decimal)

-- | Wrapped path.
newtype UrlPath = UrlPath { unUrlPath :: [UrlEncoded] } deriving (Show, Eq, Ord)

fromUrlPath :: UrlPath -> T.Text
fromUrlPath = fromUrlPathWith urlDecode (T.intercalate "/")

fromUrlPathBS :: UrlPath -> SBS.ByteString
fromUrlPathBS = fromUrlPathWith urlDecodeBS (SBS.intercalate "/")

fromUrlPathWith :: (UrlEncoded -> a) ->
                   ([a] -> a) ->
                   UrlPath -> a
fromUrlPathWith dec inter = inter . L.map dec . unUrlPath

toUrlPath :: T.Text -> UrlPath
toUrlPath = toUrlPathWith urlEncode (T.splitOn "/")

toUrlPathBS :: SBS.ByteString -> UrlPath
toUrlPathBS = toUrlPathWith urlEncodeBS (SBS.split 0x2f)

toUrlPathWith :: (Eq a, IsString a) =>
                 (a -> UrlEncoded) ->
                 (a -> [a]) ->
                 a -> UrlPath
toUrlPathWith enc spl = UrlPath . L.map enc . L.filter notRelative . spl

(</>) :: UrlPath -> UrlPath -> UrlPath
x </> y = UrlPath $ unUrlPath x ++ unUrlPath y

-- | Inject parameters to a path represented with colon or braces.
inject :: PathParams -> [(T.Text, T.Text)] -> Either Text UrlPath
inject (PathParams params) args =
  let
    f param =
      case param of
        Raw r -> Right $ urlEncode r
        Param p -> case L.lookup p args of
          Just a  -> Right $ urlEncode a
          Nothing -> Left p
    (ls, rs) = partitionEithers $ L.map f params
  in
    if L.null ls
    then Right $ UrlPath rs
    else Left $
         "Lacks following parameters: " `T.append` T.intercalate ", " ls

-- | URL with parameters.
newtype PathParams = PathParams { unPathParams :: [Piece] } deriving (Show, Eq, Ord)

data Piece = Raw T.Text | Param T.Text deriving (Show, Eq, Ord)

instance ToJSON PathParams where
  toJSON = String . fromPathParams

instance FromJSON PathParams where
  parseJSON = withText "PathParams" $
    \t -> case toPathParams t of
      Right r -> return r
      Left l  -> fail $ T.unpack l

instance Interpret PathParams where
  autoWith _ = Dhall.Type {..}
    where
      extract (TextLit (Chunks [] t)) =
        (either (const Nothing) Just . toPathParams) t
      extract  _                      = AP.empty

      expected = Text

-- | Build path with colon parameter.
fromPathParams :: PathParams -> T.Text
fromPathParams =
  let
    f x = case x of
      Raw t   -> t
      Param t -> ':' `T.cons` t
  in
    T.intercalate "/" . L.map f . unPathParams

-- | Parse the path.
toPathParams :: T.Text -> Either T.Text PathParams
toPathParams t =
  let
    paramString =
      takeWhile1
      ( \c ->
          c /= '/' && c /= '{' && c /= '}' && c/= ':'
      )
    bracedParam = Param <$> (char '{' *> paramString <* char '}')
    colonParam = Param <$> (char ':' *> paramString)
    rawPath = Raw <$> (notRelative =|< paramString)
    segment = colonParam <|> bracedParam <|> rawPath
    p = PathParams <$> ( many (char '/') *>
                         segment `sepBy` char '/' <* many (char '/')
                       )
  in
    case parse' p t of
      Done "" ps -> Right ps
      _          -> Left "Failed parsing PathParams"

-- | Collection of URL query parameters.
--   Behaviour when duplicated query keys at URL is not defined,
--   but it makes implements complecated, so treat keys as unique in this module.
--   <https://www.w3.org/MarkUp/HTMLPlus/htmlplus_42.html W3C> details
--   how query strings to be should deal with.
type Query = HM.HashMap UrlEncoded (Maybe UrlEncoded)

-- | Build query string.
buildQuery :: Query -> T.Text
buildQuery =
  let
    kv (k, v) =
      case v of
        Just v  -> k `T.snoc` '=' `T.append` v
        Nothing -> k
  in
    T.intercalate "&" . L.map kv . fromQuery

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

-- | URL Encoded 'ByteString'.
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

instance Interpret UrlEncoded where
  autoWith _ = Dhall.Type {..}
    where
      extract (TextLit (Chunks [] t)) = pure (urlEncode t)
      extract  _                      = AP.empty
      expected = Text

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

-- Helpers
notRelative :: (IsString a, Eq a) => a -> Bool
notRelative p = p /= "." && p /= ".." && p /= ""

parse' :: Parser a -> Text -> AT.Result a
parse' p t = feed (parse p t) ""

(=|<) :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
f =|< x = x >>= (\x' -> guard (f x') >> return x')
infixr 1 =|<

subDelims :: Parser Char
subDelims = satisfy (
  \c ->
    c == '!' ||
    c == '$' ||
    c == '&' ||
    c == '\'' ||
    c == '(' ||
    c == ')' ||
    c == '*' ||
    c == '+' ||
    c == ',' ||
    c == ';' ||
    c == '='
  )

pctEncoded :: Parser Char
pctEncoded =
  let
    h = satisfy isHexDigit
    f x y = chr $ digitToInt x * 0x10 + digitToInt y
  in
    char '%' *> (f <$> h <*> h)
