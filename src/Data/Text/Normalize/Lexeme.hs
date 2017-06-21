-- | Extract token from text
module Data.Text.Normalize.Lexeme(
    lexemize
  -- * Internal
  , lexemParser
  ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Text (Text)
import Data.Text.Normalize.Token

-- | Convert text to stream of tokens
lexemize :: Text -> [Token]
lexemize = either (\msg -> error $ "lexemize: " ++ msg) id . parseOnly lexemParser

-- | Parser for tokens
lexemParser :: Parser [Token]
lexemParser = spaces *> many (markup <|> word)
  where
    spaces = many space

    markup :: Parser Token
    markup = do
      w <- takeWhile1 (\c -> not (isSpace c) && isPunctuation c)
      _ <- spaces
      pure $ TokenMarkup w

    word :: Parser Token
    word = do
      w <- takeWhile1 (\c -> not $ isSpace c || isPunctuation c)
      _ <- spaces
      pure $ TokenWord w
