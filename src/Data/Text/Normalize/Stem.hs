{-# LANGUAGE LambdaCase #-}
-- | Extracting stems from tokens
module Data.Text.Normalize.Stem(
    stem
  ) where

import Data.Text.Normalize.Token
import qualified NLP.Snowball as S
import System.IO.Unsafe (unsafePerformIO)

-- | Perform extracting of a stem
stem :: [Token] -> [Token]
stem xs = unsafePerformIO $ do
  stemmer <- S.newStemmer S.Russian
  traverse (\case
    TokenWord w -> TokenWord <$> S.stemWith stemmer w
    TokenMarkup m -> pure $ TokenMarkup m) xs
