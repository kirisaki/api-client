module Web.Api where

import Data.Text
import Network.HTTP
import Network.HTTP.Types


newtype Service = Service { unwrapService :: Text } deriving (Show, Ord, Eq, Read, Generic)
data ApiMethod = ApiMethod
