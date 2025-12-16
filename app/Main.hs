module Main where

import Helper (filterWords, suggestBestWords, suggestNewWords)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("filter" : guesses) -> do
      wordsList <- lines <$> readFile "words.txt"
      let possibleWords = filterWords guesses wordsList
      putStrLn $ "There are " ++ show (length possibleWords) ++ " possible words."
    ("best" : guesses) -> do
      wordsList <- lines <$> readFile "words.txt"
      let suggestions = suggestBestWords guesses wordsList
      mapM_ putStrLn suggestions
    ("new" : guesses) -> do
      wordsList <- lines <$> readFile "words.txt"
      let suggestions = suggestNewWords guesses wordsList
      mapM_ putStrLn suggestions
    _ -> do
      putStrLn "./helper <function> <guess1> <guess2> ..."
