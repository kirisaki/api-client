{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Internal
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Internal where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import           Data.String          (IsString)
import           Data.Text            as T

notRelative :: (IsString a, Eq a) => a -> Bool
notRelative p = p /= "." && p /= ".." && p /= ""

parse' :: Parser a -> Text -> Text -> Either Text a
parse' p e t = case feed (parse p t) "" of
  Done "" r -> Right r
  _         -> Left e

(=|<) :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
f =|< x = x >>= (\x' -> guard (f x') >> return x')
infixr 1 =|<

pchar :: Parser Char
pchar = unreserved <|>
        pctEncoded <|>
        subDelims <|>
        char ':' <|>
        char '@'

unreserved :: Parser Char
unreserved = satisfy (
  \c ->
    isAscii c &&
    isAlphaNum c ||
    c == '-' ||
    c == '.' ||
    c == '_' ||
    c == '~'
  )

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
