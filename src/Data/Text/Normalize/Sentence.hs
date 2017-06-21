-- | Split tokens by sentence boundary.
module Data.Text.Normalize.Sentence(
    sentenceSplit
  , sentenceSplitFast
  , sentenceSplitSmart
  ) where

import Control.Monad.State.Strict
import Data.Char
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Normalize.StopWord
import Data.Text.Normalize.Token
import Data.Vector (Vector)

import qualified Data.List as List
import qualified Data.List.Split as SL
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | Split by sentence boundary, default strategy
sentenceSplit :: [Token] -> [[Token]]
sentenceSplit = sentenceSplitFast

-- | The simpliest splitting strategy
sentenceSplitFast :: [Token] -> [[Token]]
sentenceSplitFast = SL.split (SL.dropFinalBlank . SL.keepDelimsR $ SL.whenElt isBoundaryToken)

-- | Splits the list of tokens to sentences with complex rules.
sentenceSplitSmart :: [Token] -> [[Token]]
sentenceSplitSmart = processBounds . detectBounds (TokenWord "")

-- | Type for abbreviatures.
type AbbrTokens = [Token]

-- | Type for boundary sign.
-- We keep information on previous and next token to decide
-- wether it have to be a sentence split.
data Boundary = Boundary {
      boundarySign :: Token
    , boundaryPrev :: Either AbbrTokens Token
    , boundaryNext :: Maybe Token
  } deriving (Eq, Show)

-- | Tokens with info on abbreviations and boundary tokens.
data BoundToken = SimpleToken Token | AbbrToken [Token] | BoundToken Boundary
  deriving (Eq, Show)

detectBounds :: Token -> [Token] -> [BoundToken]
detectBounds prevToken xs = case xs of
  [] -> []
  t:rest | isDot t && isSingleLetter prevToken -> collectAbbr prevToken t rest
  t:rest | isBoundaryToken t -> processSpecial (Right prevToken) t rest
  t:rest -> processSimple t rest
  where
    processSimple  token rest = SimpleToken token : detectBounds token rest

    processSpecial prevToken curToken rest = BoundToken bound : detectBounds curToken rest
      where
        bound = Boundary curToken prevToken (getNextToken rest)

    getNextToken xs = case xs of
      [] -> Nothing
      x : _ -> Just x

    collectAbbr :: Token -> Token -> [Token] -> [BoundToken]
    collectAbbr prevToken dotToken tokens = AbbrToken abbr : BoundToken bound : rec
      where
        (abbrTail, rest) = takeAbbr tokens
        abbr = prevToken : dotToken : abbrTail
        bound = Boundary lastDot (Left abbr) (getNextToken rest)

        lastDot = List.last abbr

        rec = if null rest then [] else detectBounds lastDot rest

        takeAbbr :: [Token] -> ([Token], [Token])
        takeAbbr xs = case xs of
          t:maybeDot:rest | isDot maybeDot ->
            let (abbr, rest') = takeAbbr rest
            in  (t:maybeDot:abbr, rest')
          _ -> ([], xs)

    isSingleLetter :: Token -> Bool
    isSingleLetter x = case x of
      TokenWord xs | Text.length xs == 1 -> True
      _ -> False


-- | Function to check is it a true sentence boundary.
isTrueBound :: [Token] -> Boundary -> Bool
isTrueBound sentenceSoFar bound = not isPrevException && not isInitials
  where
    isPrevException =
      either (const False) isExceptionWord (boundaryPrev bound)
      && isStopDot

    isStopDot = isDot $ boundarySign bound

    isInitials =
         either isInitialsAbbr (const False) (boundaryPrev bound)
      && isStopDot

isInitialsAbbr :: [Token] -> Bool
isInitialsAbbr xs = length (filter isDot xs) < 3

isFirstCapital :: Token -> Bool
isFirstCapital = onWord $ isUpper . Text.head

isFirstNumber :: Token -> Bool
isFirstNumber = onWord $ isDigit . Text.head

isExceptionWord :: Token -> Bool
isExceptionWord = onWord $ flip Set.member exceptionWords

onWord :: (Text -> Bool) -> Token -> Bool
onWord pred x = case x of
  TokenWord text -> pred text
  _ -> False

type ListSentence = [Token]

-- | Converts tokens with boundaries to sentences.
processBounds :: [BoundToken] -> [ListSentence]
processBounds bounds = fromSt $ execState (mapM_ go bounds) $ St [] []
  where
    go :: BoundToken -> State St ()
    go x = case x of
      SimpleToken t -> modify $ saveTokenToCurrent t
      AbbrToken ts -> mapM_ (modify . saveTokenToCurrent) (stripCommonAbbrSyms ts)
      BoundToken bound -> do
        current <- fmap stCurrentSentence get
        if isTrueBound current bound
          then do
            modify $ saveTokenToCurrent (boundarySign bound)
            modify saveCurrentSentence
          else
            modify $ saveTokenToCurrent (boundarySign bound)

stripCommonAbbrSyms :: [Token] -> [Token]
stripCommonAbbrSyms xs = List.init (List.tail xs)

data St = St {
    stCurrentSentence :: ListSentence
  , stSentences :: [ListSentence] }

saveTokenToCurrent :: Token -> St -> St
saveTokenToCurrent t st = st { stCurrentSentence = t : stCurrentSentence st }

saveCurrentSentence :: St -> St
saveCurrentSentence (St current sentences) =
  St [] (current : sentences)

fromSt :: St -> [ListSentence]
fromSt (St currentSentence sentences) =
  reverse $ fmap reverse $
    if null currentSentence
      then sentences
      else currentSentence : sentences

---------------------------------------

isDot :: Token -> Bool
isDot x = case x of
  TokenMarkup "." -> True
  _ -> False

isBoundaryToken :: Token -> Bool
isBoundaryToken x = case x of
  TokenMarkup "." -> True
  TokenMarkup "!" -> True
  TokenMarkup "?" -> True
  TokenMarkup "!?" -> True
  TokenMarkup "?!" -> True
  TokenMarkup "..." -> True
  _ -> False

---------------------------------------

-- | TODO: complete list
exceptionWords :: Set Text
exceptionWords =
  Set.fromList $ fmap Text.pack [
    "mr", "mrs", "ms", "dr", "mt",
    "inc", "vol", "et", "al", "pp", "пр", "ул" ]
