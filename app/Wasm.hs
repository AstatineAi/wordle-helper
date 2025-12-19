{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedStringFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Helper (suggestBestWords, suggestNewWords)
import Language.Javascript.JSaddle (JSM, ToJSVal (..), fun, jsg, liftJSM, valToText, (<#), function, call)
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: IO ()

-- Embed the words list
wordListStr :: String
wordListStr = $(embedStringFile "words.txt")

wordList :: [String]
wordList = lines wordListStr

main :: IO ()
main = JSaddle.Wasm.run $ do
  liftIO $ putStrLn "Haskell main started"
  window <- jsg ("window" :: Text)

  -- suggestNewWords
  suggestNewWordsFunc <- function $ \_ _ args -> do
      liftIO $ putStrLn "suggestNewWords called"
      case args of
        [inputVal, callback] -> do
          inputTxt <- valToText inputVal
          let inputBs = LBS.fromStrict $ TE.encodeUtf8 inputTxt
          let guesses = fromMaybe [] (decode inputBs :: Maybe [String])
          let result = suggestNewWords guesses wordList
          let resultJson = encode result
          let resultTxt = TE.decodeUtf8 $ LBS.toStrict resultJson
          _ <- call callback callback [resultTxt]
          return ()
        _ -> return ()
  
  window <# ("suggestNewWords" :: Text) $ suggestNewWordsFunc

  -- suggestBestWords
  suggestBestWordsFunc <- function $ \_ _ args -> do
      case args of
        [inputVal, callback] -> do
          inputTxt <- valToText inputVal
          let inputBs = LBS.fromStrict $ TE.encodeUtf8 inputTxt
          let guesses = fromMaybe [] (decode inputBs :: Maybe [String])
          let result = suggestBestWords guesses wordList
          let resultJson = encode result
          let resultTxt = TE.decodeUtf8 $ LBS.toStrict resultJson
          _ <- call callback callback [resultTxt]
          return ()
        _ -> return ()

  window <# ("suggestBestWords" :: Text) $ suggestBestWordsFunc

  return ()
