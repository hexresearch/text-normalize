module Data.Text.NormalizeSpec( spec ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec
import Test.HUnit

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Data.Text.Normalize

spec :: Spec
spec = do
  it "Normalize empty doc" $
    assertEqual "empty" [] (normalizeSimple 100 "")
  it "Normalize single word" $ do
    assertEqual "word" ["обыкновен"] (norm "обыкновенный")
    assertEqual "case insensitive" ["обыкновен"] (norm "Обыкновенный")
    assertEqual "word2" ["широк"] (norm "широкий")
  it "Normalize sentence" $ do
    assertEqual "sentence1" ["пьер", "одел", ".", "вышел", "здан", "."] $ norm "Пьер оделся. Он вышел из здания."
    assertEqual "sentence2" [["пьер", "одел", "."], ["вышел", "здан", "."]] $ normalizeSimple 1 "Пьер оделся. Он вышел из здания."
    assertEqual "sentence3" ["пьер", "одел", ".", "вышел", "здан"] $ norm "Пьер оделся. Он вышел из здания"
    assertEqual "sentence3" ["пьер", "одел", "?", "вышел", "здан", "!"] $ norm "Пьер оделся? Он вышел из здания!"
    assertEqual "sentence3" ["пьер", "одел", "?!", "вышел", "здан", "!?"] $ norm "Пьер оделся?! Он вышел из здания!?"
    assertEqual "sentence4" ["пьер", "одел", "...", "вышел", "здан", "..."] $ norm "Пьер оделся... Он вышел из здания..."
  it "Normalize quotation" $ do
    assertEqual "quotation1" ["«", "журна", "«", "знан", ".", "умен", "»"] $ norm "«Журнал «Знание. Умение»"
    assertEqual "quotation2" [["«", "журна", "«", "знан", "."], ["умен", "»"]] $ normalizeSimple 1 "«Журнал «Знание. Умение»"

norm :: Text -> [Text]
norm = head . normalizeSimple 100
