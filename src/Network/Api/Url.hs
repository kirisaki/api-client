{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Url
  (
    inject
  ) where

import           Network.Api.Parser

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text

-- | Inject parameters to a path represented with colon or braces.
inject :: Text -> [(Text, Text)] -> Either Text Text
inject path params =
  let
    inject' (Right path, "") = Right path
    inject' (Left e, _) = Left e
    inject' (Right path, remain) =
      case feed (parse segment remain) "" of
        Done rem new ->
          case new of
            Param k ->
              case lookup k params of
                Just v ->
                  inject' (Right (path `snoc` '/' `append` v), rem)
                Nothing ->
                  Left "lack parameters"
            Raw "" ->
              inject' (Right path, rem)
            Raw t ->
              inject' (Right (path `snoc` '/' `append` t), rem)
        _ ->
          Left "failed parsing"
  in
    inject' (Right "", path)
