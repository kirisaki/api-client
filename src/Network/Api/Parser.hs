module Network.Api.Parser
  (
    Segment(..)
  , segment
  )
where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text

-- Parsers
bracedParam :: Parser Segment
bracedParam = Param <$> (char '{' *> takeTill (== '}') <* char '}')

colonParam :: Parser Segment
colonParam = Param <$> (char ':' *> takeTill (== '/'))

rawPath :: Parser Segment
rawPath = Raw <$> (takeTill (== '/') <|> takeText)

data Segment = Param Text | Raw Text deriving(Eq, Show)

segment :: Parser Segment
segment =  skipWhile (== '/') *> (colonParam <|> bracedParam <|> rawPath) <* option '/' (char '/')
