{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Service
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Service
  (
    -- * Service
    Service(..)
  , Endpoint(..)
  , HttpVersion(..)
  , Method(..)
    -- * Path with parameters
  , inject
  , PathParams(..)
  , Piece(..)
  , buildPathParams
  , parsePathParams
  ) where

import           Network.Api.Header
import           Network.Api.Internal
import           Network.Api.Url

import           Control.Applicative  as AP
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Either
import           Data.List            as L
import qualified Data.Text            as T
import           Data.Word
import qualified Dhall                as DH
import qualified Dhall.Core           as DHC
import           GHC.Generics
import           Numeric.Natural

-- | API Definition
data Service = Service
  { baseUrl           :: Url
  , endpoints         :: [Endpoint]
  , httpVersion       :: HttpVersion
  , defaultHeader     :: Header
  , tokenHeaderName   :: Maybe FieldName
  , tokenHeaderPrefix :: Maybe T.Text
  , tokenQueryName    :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON Service
instance ToJSON Service
instance DH.Interpret Service

-- | Version of HTTP.
--   You don't think numbers of version take negative value, do you?
data HttpVersion = HttpVersion
  { majorVersion :: Natural
  , minorVersion :: Natural
  } deriving (Eq, Ord)

instance Show HttpVersion where
  show (HttpVersion major minor) = show major ++ "." ++ show minor

instance ToJSON HttpVersion where
  toJSON = String . T.pack . show

instance FromJSON HttpVersion where
  parseJSON =
      withText "HttpVersion" $
      \t ->
        case feed (parse httpVersionP t) "" of
          Done "" v -> pure v
          _         -> pure (HttpVersion 1 1)

instance DH.Interpret HttpVersion where
  autoWith _ = DH.Type {..}
    where
      extract (DHC.TextLit (DHC.Chunks [] t)) =
        case feed (parse httpVersionP t) "" of
          Done "" v -> Just v
          _         -> Nothing
      extract  _                      = AP.empty

      expected = DHC.Text

httpVersionP :: Parser HttpVersion
httpVersionP = HttpVersion <$> decimal <* char '.' <*> decimal

-- | API Endpoint definition
data Endpoint = Endpoint
  { endpointMethod :: Method
  , endpointPath   :: PathParams
  } deriving (Eq, Show, Generic)

instance FromJSON Endpoint
instance ToJSON Endpoint
instance DH.Interpret Endpoint

-- | HTTP method
data Method
  = GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OPTIONS
  | PATCH
  | Custom T.Text
  deriving (Show, Ord, Eq, Read, Generic)
instance FromJSON Method
instance ToJSON Method
instance DH.Interpret Method

-- | Inject parameters to a path represented with colon or braces.
inject :: PathParams -> [(T.Text, T.Text)] -> Either T.Text UrlPath
inject (PathParams params) args =
  let
    f param =
      case param of
        Raw r -> Right r
        Param p -> case L.lookup p args of
          Just a  -> Right a
          Nothing -> Left p
    (ls, rs) = partitionEithers $ L.map f params
  in
    if L.null ls
    then Right $ toUrlPath rs
    else Left $
         "Lacks following parameters: " `T.append` T.intercalate ", " ls

-- | URL with parameters.
newtype PathParams = PathParams
  { unPathParams :: [Piece] } deriving (Show, Eq, Ord)

data Piece = Raw T.Text | Param T.Text deriving (Show, Eq, Ord)

instance ToJSON PathParams where
  toJSON = String . buildPathParams

instance FromJSON PathParams where
  parseJSON = withText "PathParams" $
    \t -> case parsePathParams t of
      Right r -> return r
      Left l  -> fail $ T.unpack l

instance DH.Interpret PathParams where
  autoWith _ = DH.Type {..}
    where
      extract (DHC.TextLit (DHC.Chunks [] t)) =
        (either (const Nothing) Just . parsePathParams) t
      extract  _                      = AP.empty

      expected = DHC.Text

-- | Build path with colon parameter.
buildPathParams :: PathParams -> T.Text
buildPathParams =
  let
    f x = case x of
      Raw t   -> t
      Param t -> ':' `T.cons` t
  in
    T.intercalate "/" . L.map f . unPathParams

-- | Parse the path.
parsePathParams :: T.Text -> Either T.Text PathParams
parsePathParams =
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
    parse' p "Failed parsing PathParams"
