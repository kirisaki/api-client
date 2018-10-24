module TestUtils where

import           Data.Aeson           as AE
import           Data.Attoparsec.Text as AT

-- Helpers for Aeson.
isError :: AE.Result a -> Bool
isError (Error _) = True
isError _         = False

isSuccess :: AE.Result a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

success :: AE.Result a -> a
success (Success a) = a

error_ :: AE.Result a -> String
error_ (Error e) = e

--Unsafe helpers for tests
right :: Either a b -> b
right (Right b)  = b

left :: Either a b -> a
left (Left a) = a

--Unsafe helpers for Attoparsec.
done :: AT.Result a -> a
done (Done _ x) = x
