module Data.Text.HistogrammSpec( spec ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec
import Test.HUnit

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Data.Text.Normalize
import Data.Text.Histogramm

spec :: Spec
spec =
  it "calculates a histogramm" $
    assertEqual "histogramm" resHist (histFromList $ concat chunk) 
    where
      chunk :: [[Token]]
      chunk = [ch1, ch2, ch3]
      ch1 = [TokenWord "a", TokenWord "b"]
      ch2 = ch1 ++ [TokenWord "c"]
      ch3 = ch2 ++ [TokenWord "d"]
      resHist = Histogramm $ M.fromList [(TokenWord "a", 3), (TokenWord "b", 3), (TokenWord "c", 2), (TokenWord "d", 1)]
