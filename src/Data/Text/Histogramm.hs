-- | Word frequency analysis for text
module Data.Text.Histogramm(
    Histogramm(..)
  , getHistogramm
  , histFromList
  , getSlidingWindowHistogramm
  , removeNoizyWords
  , histAdd
  , pointScale
  , normalizeHist
  , distanceHist
  , dotHist
  , lengthHist
  , makeGaussKernell
  , nullHist
  ) where

import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Histogramm shows how many times each unique
-- value is contained within a container.
newtype Histogramm a = Histogramm { unHistogramm :: Map a Double }
  deriving (Show, Eq, Ord, Generic)

-- | Creates histogramm of words for a given chunk.
getHistogramm :: (Foldable f, Ord a)
  => Double -- ^ Threshold, values that less than the value are dropped
  -> f a -- ^ Input data
  -> Histogramm a
getHistogramm th chunk = normalizeHist $ removeNoizyWords th $ histFromList chunk

-- | Creates a histogramm out of list of values.
histFromList :: (Foldable f, Ord a) => f a -> Histogramm a
histFromList = Histogramm . M.fromList . fmap getCount . L.group . L.sort . F.toList
  where
    getCount xs = count `seq` (token, count)
      where
        token = L.head xs
        count = fromIntegral $ length xs

-- | Creates histogramm of N-letters. It slides along the words
-- with overlapping windows by N-letters and creates the histogramm out of them.
getSlidingWindowHistogramm :: forall f . Foldable f => Int -> f Text -> Histogramm Text
getSlidingWindowHistogramm n chunk =
  normalizeHist $ removeNoizyWords 10 $
  histFromList $ fmap T.pack $ L.divvy n 1 $ getTextAsList chunk
  where
    getTextAsList :: f Text -> String
    getTextAsList = T.unpack . T.concat . F.toList

-- | Removes all words that have score less than given value.
removeNoizyWords :: Double -> Histogramm a -> Histogramm a
removeNoizyWords level (Histogramm m) = Histogramm $ M.filter (> level) m

-- | Summ values of two histogramms
histAdd :: Ord a => Histogramm a -> Histogramm a -> Histogramm a
histAdd (Histogramm ma) (Histogramm mb) = Histogramm $ M.unionWith (+) ma mb

-- | Scale histogramm by a value
pointScale :: Double -> Histogramm a -> Histogramm a
pointScale k (Histogramm hist) = Histogramm $ fmap (* k) hist

-- | Scales all components of the hist by it's total length.
normalizeHist :: Ord a => Histogramm a -> Histogramm a
normalizeHist x = pointScale factor x
  where factor = recip $ lengthHist x

-- | Calculate Euclidian distance between two histogramms
distanceHist :: Ord a => Histogramm a -> Histogramm a -> Double
distanceHist a b = lengthHist $ histAdd a (pointScale (-1) b)

-- | Scalar product for histogramms.
dotHist :: Ord a => Histogramm a -> Histogramm a -> Double
dotHist (Histogramm ma) (Histogramm mb) =
  sum $ M.intersectionWith (*) ma mb

-- | Length of the Histogram as a vector.
lengthHist :: Histogramm a -> Double
lengthHist (Histogramm ma) =
  sqrt $ getSum $ foldMap (\x -> Sum $ x * x) ma

-- | Creates the Gauss kernell out of addition, scaling and length operations.
makeGaussKernell :: (a -> a -> a) -> (Double -> a -> a) -> (a -> Double) -> (a -> a -> Double)
makeGaussKernell add scale len a b =
  exp $ negate $ 0.5 * (len (add a (scale (-1) b)) ** 2)

-- | Return 'True' if histogramm doesn't contains any value
nullHist :: Histogramm a -> Bool
nullHist (Histogramm values) = M.null values
