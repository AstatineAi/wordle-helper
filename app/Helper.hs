{-# LANGUAGE ImportQualifiedPost #-}

module Helper where

import Data.Char (isAlpha, toLower)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)

data Color = Green | Yellow | Gray deriving (Eq, Show)

type Guess = (String, [Color])

data Position = Correct | Incorrect | Unknown deriving (Eq, Show)

type LetterState = (Position, Position, Position, Position, Position)

type WordleState = M.Map Char LetterState

getCharScore :: Char -> Int
getCharScore c = case elemIndex c frequencyOrder of
  Just idx -> 26 - idx
  Nothing -> error $ "Character not found in frequency order: " ++ [c]
  where
    frequencyOrder = "etaoinshrdlcumwfgypbvkjxqz"

parseColor :: Char -> Color
parseColor 'g' = Green
parseColor 'y' = Yellow
parseColor _ = Gray

-- Pattern: apple:yyygg means guess "apple" and get results
-- yellow, yellow, green, gray, gray
parseGuess :: String -> Maybe Guess
parseGuess s'
  | length word == 5 && length colors == 5 = Just (word, colors)
  | otherwise = Nothing
  where
    s = filter isGuessChar s'
    isGuessChar c = isAlpha c || c == ':'
    (wordPart, colorPart) = span (/= ':') s
    word = map toLower wordPart
    colors = map parseColor (tail colorPart)

-- WordleState contains states for each letter
-- LetterState is a list of 5 Position, describing the status of that letter
-- at each position in the word
toWordleState :: [Guess] -> WordleState
toWordleState = foldl updateGuess initialState
  where
    initialState = M.fromList [(c, (Unknown, Unknown, Unknown, Unknown, Unknown)) | c <- ['a' .. 'z']]

    updateGuess :: WordleState -> Guess -> WordleState
    updateGuess state (word, colors) = foldl updateLetter state (zip3 word colors [0 .. 4])

    -- Gray: set all positions to Incorrect
    -- Yellow: set that specific position to Incorrect, others unchanged
    -- Green: set that specific position to Correct, others unchanged
    updateLetter :: WordleState -> (Char, Color, Int) -> WordleState
    updateLetter state (c, color, pos)
      | color == Green = M.adjust (setCorrect pos) c state
      | color == Yellow = M.adjust (setIncorrect pos) c state
      | color == Gray = M.adjust setAbsent c state
      | otherwise = state
      where
        setCorrect p positions =
          updatePosition positions p Correct
        setIncorrect p positions =
          updatePosition positions p Incorrect
        setAbsent _ =
          (Incorrect, Incorrect, Incorrect, Incorrect, Incorrect)

        updatePosition :: (Position, Position, Position, Position, Position) -> Int -> Position -> (Position, Position, Position, Position, Position)
        updatePosition (_, p1, p2, p3, p4) 0 newPos = (newPos, p1, p2, p3, p4)
        updatePosition (p0, _, p2, p3, p4) 1 newPos = (p0, newPos, p2, p3, p4)
        updatePosition (p0, p1, _, p3, p4) 2 newPos = (p0, p1, newPos, p3, p4)
        updatePosition (p0, p1, p2, _, p4) 3 newPos = (p0, p1, p2, newPos, p4)
        updatePosition (p0, p1, p2, p3, _) 4 newPos = (p0, p1, p2, p3, newPos)
        updatePosition tuple _ _ = tuple

isPossibleWord :: WordleState -> String -> Bool
isPossibleWord state word = all isPossibleLetter (zip word [0 .. 4])
  where
    isPossibleLetter :: (Char, Int) -> Bool
    isPossibleLetter (c, pos) =
      case M.lookup c state of
        Just letterState -> isPositionPossible letterState pos
        Nothing -> False

    isPositionPossible :: LetterState -> Int -> Bool
    isPositionPossible (p0, p1, p2, p3, p4) position =
      case position of
        0 -> p0 /= Incorrect
        1 -> p1 /= Incorrect
        2 -> p2 /= Incorrect
        3 -> p3 /= Incorrect
        4 -> p4 /= Incorrect
        _ -> False

-- Best words: most likely to be the answer
-- New words: most likely to eliminate possibilities

rateBestWord :: WordleState -> String -> Double
rateBestWord = error "Not implemented"

rateNewWord :: WordleState -> String -> Double
rateNewWord = error "Not implemented"

suggestBestWords :: [String] -> [String] -> [String]
suggestBestWords rawGuesses possibleWords = take 10 $ sortOn (rateBestWord wordleState) possibleWords
  where
    guesses = mapMaybe parseGuess rawGuesses
    wordleState = toWordleState guesses

suggestNewWords :: [String] -> [String] -> [String]
suggestNewWords rawGuesses possibleWords = take 10 $ sortOn (rateNewWord wordleState) possibleWords
  where
    guesses = mapMaybe parseGuess rawGuesses
    wordleState = toWordleState guesses

filterWords :: [String] -> [String] -> [String]
filterWords rawGuesses = filter (isPossibleWord state)
  where
    guesses = mapMaybe parseGuess rawGuesses
    state = toWordleState guesses
