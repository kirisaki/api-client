module TestUtils where

import           Data.Aeson

-- Misc for aeson
isError :: Result a -> Bool
isError (Error _) = True
isError _         = False

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

--Unsafe helper for tests
right :: Either a b -> b
right (Right b)  = b

left :: Either a b -> a
left (Left a) = a

