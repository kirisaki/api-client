module TestUtils where

import           Data.Aeson

-- Helpers for Aeson.
isError :: Result a -> Bool
isError (Error _) = True
isError _         = False

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

success :: Result a -> a
success (Success a) = a

error_ :: Result a -> String
error_ (Error e) = e

--Unsafe helper for tests
right :: Either a b -> b
right (Right b)  = b

left :: Either a b -> a
left (Left a) = a

