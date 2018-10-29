{-# LANGUAGE OverloadedStrings #-}
module TestUtils where

import           Data.Aeson           as AE
import           Data.Attoparsec.Text as AT
import           Data.Char
import qualified Data.List            as L
import qualified Data.Text            as T
import           Network.Api.Internal
import           Test.QuickCheck

-- Helpers for Aeson.
isError :: AE.Result a -> Bool
isError (AE.Error _) = True
isError _            = False

isSuccess :: AE.Result a -> Bool
isSuccess (AE.Success _) = True
isSuccess _              = False

success :: AE.Result a -> a
success (AE.Success a) = a

error_ :: AE.Result a -> String
error_ (AE.Error e) = e

-- Unsafe helpers for tests
right :: Either a b -> b
right (Right b)  = b

left :: Either a b -> a
left (Left a) = a

-- Unsafe helpers for Attoparsec.
done :: AT.Result a -> a
done (Done _ x) = x

-- Arbitrary instances
newtype PathText = PathText { pathText :: T.Text } deriving (Show, Ord, Eq)
instance Arbitrary PathText where
  arbitrary =
    let
      pathChar = arbitrary
        `suchThat` (\c ->
                      c /= '{' &&
                      c /= '}' &&
                      c /= '/' &&
                      c /= ':')
      paramText = listOf1 pathChar `suchThat` notRelative
      colonParam = (:) ':' <$> paramText
      bracedParam = (\s -> "{" ++ s ++ "}") <$> paramText
      segment = oneof [colonParam, bracedParam, paramText]
    in
      PathText . T.intercalate "/" . L.map T.pack  <$> listOf segment
  shrink (PathText "") = []
  shrink (PathText path) = ( shrink .
                             PathText .
                             T.drop 1 .
                             T.dropWhile (/= '/')
                           ) path
newtype Unreserved = Unreserved
  { unreserved :: Char } deriving (Show, Eq, Ord)
instance Arbitrary Unreserved where
  arbitrary = Unreserved <$> arbitrary `suchThat`
    (\c -> isAlphaNum c ||
           c == '-' ||
           c == '.' ||
           c == '_' ||
           c == '~')

newtype SubDelims = SubDelims
  { subDelims :: Char } deriving (Show, Eq, Ord)
instance Arbitrary SubDelims where
  arbitrary = SubDelims <$> elements ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

newtype PctEncoded = PctEncoded
  { pctEncoded :: T.Text } deriving (Show, Eq, Ord)
instance Arbitrary PctEncoded where
  arbitrary =
    let
      pct = elements "%"
      hex = elements "0123456789ABCDEF"
    in
      (\a b c -> PctEncoded $ T.pack [a, b, c]) <$> pct <*> hex <*> hex
