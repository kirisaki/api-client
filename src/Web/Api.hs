{-# LANGUAGE DeriveGeneric #-}
module Web.Api where

import Data.Text
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types


newtype Service = Service { unwrapService :: Text } deriving (Show, Ord, Eq, Read, Generic)
data ApiMethod = ApiMethod
