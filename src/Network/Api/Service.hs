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
  , Method(..)
  , HttpMethod(..)
    -- * Path with parameters
  , inject
  , PathParams(..)
  , Piece(..)
  , fromPathParams
  , toPathParams
  ) where

import           Network.Api.Header
import           Network.Api.Internal
import           Network.Api.Url

import           Control.Applicative  as AP
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Either
import           Data.List            as L
import qualified Data.Text            as T
import           Dhall                hiding (inject)
import           Dhall.Core
import           GHC.Generics

-- | API Definition
data Service = Service
  { baseUrl           :: T.Text
  , methods           :: [Method]
  , defaultHeader     :: Header
  , tokenHeaderName   :: Maybe FieldName
  , tokenHeaderPrefix :: Maybe T.Text
  , tokenQueryName    :: Maybe T.Text
  } deriving (Eq, Show, Ord, Generic)
instance FromJSON Service
instance ToJSON Service

-- | API definition
data Method = Method
  { httpMethod  :: HttpMethod
  , apiEndpoint :: T.Text
  } deriving (Eq, Show, Ord, Read, Generic)

instance FromJSON Method
instance ToJSON Method

-- | HTTP method
data HttpMethod
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
instance FromJSON HttpMethod
instance ToJSON HttpMethod

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
    then Right $ toUrlPathFromList rs
    else Left $
         "Lacks following parameters: " `T.append` T.intercalate ", " ls

-- | URL with parameters.
newtype PathParams = PathParams
  { unPathParams :: [Piece] } deriving (Show, Eq, Ord)

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
toPathParams =
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
