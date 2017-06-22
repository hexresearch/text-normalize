module Main where

import Data.Text.Normalize
import System.Environment
import Data.Foldable (traverse_)

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    [p] -> processDoc p
    _ -> putStrLn "Expected single argument: path to file to normalize"

processDoc :: FilePath -> IO ()
processDoc p = do
  chunks <- normalizeWords 100 <$> T.readFile p
  putStrLn "Normalized chunks:"
  traverse_ (T.putStrLn . T.unwords) chunks
