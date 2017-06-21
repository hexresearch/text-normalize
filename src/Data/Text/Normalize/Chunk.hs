-- | Splitting token stream to chunks
module Data.Text.Normalize.Chunk(
    splitChunks
  ) where

import Data.Text.Normalize.Token

import qualified Data.Foldable as F

-- | Take sentences and merge them into chunks of desired size
splitChunks :: Int -> [[Token]] -> [[Token]]
splitChunks n = reverse . fst . F.foldl' go ([], 0)
  where
    go :: ([[Token]], Int) -> [Token] -> ([[Token]], Int)
    go (acc, i) s
      | i < n = case acc of
        [] -> ([s], i + length s)
        (c : cs) -> ((c ++ s) : cs, i + length s)
      | otherwise = go ([] : acc, 0) s
