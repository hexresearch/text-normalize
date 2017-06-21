-- | Token definition and manipulation
module Data.Text.Normalize.Token(
    Token(..)
  , filterWords
  , isTokenMarkup
  , isTokenWord
  , leaveWords
  , mapWord
  , tokenValue
  ) where

import Data.Text (Text)
import GHC.Generics

import qualified Data.Text as T

-- | Text is splitted to stream of tokens
data Token =
    TokenWord !Text -- ^ Simple word
  | TokenMarkup !Text -- ^ Commas, dots and etc.
  deriving (Eq, Ord, Generic)

-- | Do not escapes non-latin
instance Show Token where
  show t = case t of
    TokenWord v -> "TokenWord " ++ T.unpack v
    TokenMarkup v -> "TokenMarkup " ++ T.unpack v

-- | Is token is word
isTokenWord :: Token -> Bool
isTokenWord TokenWord{} = True
isTokenWord _ = False

-- | Is token is punctuation
isTokenMarkup :: Token -> Bool
isTokenMarkup TokenMarkup{} = True
isTokenMarkup _ = False

-- | Unwrap token to text
tokenValue :: Token -> Text
tokenValue (TokenWord v) = v
tokenValue (TokenMarkup v) = v

-- | Leave only satisfying words in stream
filterWords :: (Text -> Bool) -> [Token] -> [Token]
filterWords f = filter $ \case
  TokenWord v -> f v
  TokenMarkup{} -> True

-- | Map words in token
mapWord :: (Text -> Text) -> Token -> Token
mapWord f (TokenWord v) = TokenWord (f v)
mapWord _ w = w

-- | Leave only words in stream, drop punctuation
leaveWords :: [Token] -> [Text]
leaveWords = fmap tokenValue . filter isTokenWord
