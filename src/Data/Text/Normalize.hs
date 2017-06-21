module Data.Text.Normalize(
  -- * Simple API
    Token(..)
  , normalize
  , normalizeSimple
  , normalizeWords
  -- * Fine grained control API
  , lexemize
  , stem
  , sentenceSplit
  , splitChunks
  , dropStopWords
  ) where

import Data.Text (Text)
import Data.Text.Normalize.Chunk
import Data.Text.Normalize.Lexeme
import Data.Text.Normalize.Sentence
import Data.Text.Normalize.Stem
import Data.Text.Normalize.StopWord
import Data.Text.Normalize.Token

import qualified Data.Text as T

-- | Perform normalization all-in (including splitting into chunks).
--
-- Returns list of chunks, each chunk is a list of tokens.
normalize :: Int -- ^ Size of chunk in words
  -> Text -- ^ Input text
  -> [[Token]] -- ^ Chunks
normalize chunkSize = splitChunks chunkSize
  . sentenceSplit
  . fmap (mapWord T.toLower)
  . stem
  . dropStopWords
  . lexemize

-- | Perform normalization all-in (including splitting into chunks) and unwrap tokens.
--
-- Returns list of chunks, each chunk contains normalized stems without stop words.
normalizeSimple :: Int -- ^ Size of chunk in words
  -> Text -- ^ Input text
  -> [[Text]] -- ^ Chunks
normalizeSimple chunkSize = merge . normalize chunkSize
  where
    merge :: [[Token]] -> [[Text]]
    merge = fmap (fmap tokenValue)

-- | Perform normalization all-in (including splitting into chunks) and unwrap tokens and drop punctuation.
--
-- Returns list of chunks, each chunk contains normalized stems without stop words and punctuation.
normalizeWords :: Int -- ^ Size of chunk in words
  -> Text -- ^ Input text
  -> [[Text]] -- ^ Chunks
normalizeWords chunkSize = merge . normalize chunkSize
  where
    merge :: [[Token]] -> [[Text]]
    merge = fmap (fmap tokenValue . filter isTokenWord)
