{-# LANGUAGE DeriveGeneric  #-}
module Network.Api.Service
  ( Service(..)
  , Method(..)
  , HttpMethod(..)
  ) where

import Network.Api.Header

import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Text
import GHC.Generics

-- | API Definition
data Service = Service
  { baseUrl           :: Text
  , methods           :: [Method]
  , defaultHeader     :: [(Text, Text)]
  , tokenHeaderName   :: Maybe Text
  , tokenHeaderPrefix :: Maybe Text
  , tokenQueryName    :: Maybe Text
  } deriving (Eq, Show, Ord, Read, Generic)
instance FromJSON Service
instance ToJSON Service

-- | API definition
data Method = Method
  { httpMethod  :: HttpMethod
  , apiEndpoint :: Text
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
  | Custom Text
  deriving (Show, Ord, Eq, Read, Generic)
instance FromJSON HttpMethod
instance ToJSON HttpMethod

