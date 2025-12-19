{-# LANGUAGE ImportQualifiedPost #-}

module Helper where

import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))

data Color = Green | Yellow | Gray deriving (Eq, Show, Ord)

type GuessColor = (Color, Color, Color, Color, Color)

type Guess = (String, GuessColor)

data Position = Correct | Incorrect | Unknown deriving (Eq, Show)

-- LetterState: (isPresentInWord, (pos0, pos1, pos2, pos3, pos4))
-- TODO: Should count occurrences of letters in case of multiple same letters
type LetterState = (Bool, (Position, Position, Position, Position, Position))

type WordleState = M.Map Char LetterState

parseColor :: Char -> Color
parseColor 'g' = Green
parseColor 'y' = Yellow
parseColor _ = Gray

colorsToList :: GuessColor -> [Color]
colorsToList (c0, c1, c2, c3, c4) = [c0, c1, c2, c3, c4]

listToColors :: [Color] -> GuessColor
listToColors [c0, c1, c2, c3, c4] = (c0, c1, c2, c3, c4)
listToColors _ = error "List must have exactly 5 colors"

-- Pattern: apple:yyygg means guess "apple" and get results
-- yellow, yellow, green, gray, gray
parseGuess :: String -> Maybe Guess
parseGuess s'
  | length word == 5 && length colors == 5 = Just (word, listToColors colors)
  | otherwise = Nothing
  where
    s = filter isGuessChar s'
    isGuessChar c = isAlpha c || c == ':'
    (wordPart, colorPart) = span (/= ':') s
    word = map toLower wordPart
    colors = if null colorPart then [] else map parseColor (drop 1 colorPart)

-- WordleState contains states for each letter
-- LetterState is a list of 5 Position, describing the status of that letter
-- at each position in the word
toWordleState :: [Guess] -> WordleState
toWordleState = foldl updateGuess initialState
  where
    initialState = M.fromList [(c, (False, (Unknown, Unknown, Unknown, Unknown, Unknown))) | c <- ['a' .. 'z']]

    updateGuess :: WordleState -> Guess -> WordleState
    updateGuess state (word, colors) = foldl updateLetter state (zip3 word (colorsToList colors) [0 .. 4])
      where
        activeLetters :: [Char]
        activeLetters = [c | (c, color) <- zip word (colorsToList colors), color /= Gray]

        -- Gray: set all positions to Incorrect
        -- Yellow: set that specific position to Incorrect, others unchanged
        -- Green: set that specific position to Correct, others unchanged
        updateLetter :: WordleState -> (Char, Color, Int) -> WordleState
        updateLetter s (c, color, pos)
          | color == Green =
              M.adjust setPresent c $ M.adjust (setCorrect pos) c $ M.map (setIncorrect pos) s
          | color == Yellow =
              M.adjust setPresent c $ M.adjust (setIncorrect pos) c s
          | color == Gray =
              if c `elem` activeLetters
                then
                  M.adjust (setIncorrect pos) c s
                else
                  M.adjust setAbsent c s
          | otherwise = s
    setCorrect p (isPresent, positions) = (isPresent, updatePosition positions p Correct)
    setIncorrect p (isPresent, positions) = (isPresent, updatePosition positions p Incorrect)
    setAbsent _ = (False, (Incorrect, Incorrect, Incorrect, Incorrect, Incorrect))
    setPresent (_, positions) = (True, positions)

    updatePosition :: (Position, Position, Position, Position, Position) -> Int -> Position -> (Position, Position, Position, Position, Position)
    updatePosition (_, p1, p2, p3, p4) 0 newPos = (newPos, p1, p2, p3, p4)
    updatePosition (p0, _, p2, p3, p4) 1 newPos = (p0, newPos, p2, p3, p4)
    updatePosition (p0, p1, _, p3, p4) 2 newPos = (p0, p1, newPos, p3, p4)
    updatePosition (p0, p1, p2, _, p4) 3 newPos = (p0, p1, p2, newPos, p4)
    updatePosition (p0, p1, p2, p3, _) 4 newPos = (p0, p1, p2, p3, newPos)
    updatePosition tuple _ _ = tuple

isPossibleWord :: WordleState -> String -> Bool
isPossibleWord state word = all isPossibleLetter (zip word [0 .. 4]) && all (`elem` word) mustPresentLetters
  where
    mustPresentLetters = [c | (c, _) <- M.toList state, mustLetterPresent state c]
    isPossibleLetter :: (Char, Int) -> Bool
    isPossibleLetter (c, pos) =
      case M.lookup c state of
        Just letterState -> isPositionPossible letterState pos
        Nothing -> False

    isPositionPossible :: LetterState -> Int -> Bool
    isPositionPossible (_, (p0, p1, p2, p3, p4)) position =
      case position of
        0 -> p0 /= Incorrect
        1 -> p1 /= Incorrect
        2 -> p2 /= Incorrect
        3 -> p3 /= Incorrect
        4 -> p4 /= Incorrect
        _ -> False

mustLetterPresent :: WordleState -> Char -> Bool
mustLetterPresent state c =
  maybe False fst (M.lookup c state)

-- Best words: most likely to be the answer
-- Suppose your word list is sorted by frequency of usage
suggestBestWords :: [String] -> [String] -> [String]
suggestBestWords rawGuesses universe = take 10 candidates
  where
    guesses = mapMaybe parseGuess rawGuesses
    wordleState = toWordleState guesses
    candidates = filter (isPossibleWord wordleState) universe

getGuess :: String -> String -> GuessColor
getGuess word answer = listToColors colors
  where
    colors = [getColor w a | (w, a) <- zip word answer]
    getColor w a
      | w == a = Green
      | w `elem` answer = Yellow
      | otherwise = Gray

calculateEntropy :: [String] -> String -> Double
calculateEntropy candidates guess = sum [- (p * logBase 2 p) | p <- probabilities, p > 0]
  where
    total = fromIntegral (length candidates) :: Double
    patternCounts = M.fromListWith (+) [(getGuess guess answer, 1 :: Int) | answer <- candidates]
    probabilities = [fromIntegral count / total | count <- M.elems patternCounts]

-- New words: most likely to eliminate possibilities
suggestNewWords :: [String] -> [String] -> [String]
suggestNewWords rawGuesses universe = map fst $ take 100 $ sortOn (Down . snd) results
  where
    guesses = mapMaybe parseGuess rawGuesses
    wordleState = toWordleState guesses
    candidates = filter (isPossibleWord wordleState) universe
    searchSpace = if length candidates < 50 then candidates else universe
    calcTask = map (\word -> (word, calculateEntropy candidates word)) searchSpace
    results = calcTask `using` parListChunk 50 rdeepseq

filterWords :: [String] -> [String] -> [String]
filterWords rawGuesses = filter (isPossibleWord state)
  where
    guesses = mapMaybe parseGuess rawGuesses
    state = toWordleState guesses
