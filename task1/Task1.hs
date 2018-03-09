{-# LANGUAGE LambdaCase #-}

module Main where

import           LambdaCalculus
import           Parser
import           Util

import           Text.Megaparsec.Error (parseErrorPretty)

import qualified Data.Text.IO          as T
import           System.IO


main :: IO ()
main = io $ \input output -> parseLambda <$> T.hGetContents input >>= \case
    Left err -> hPutStr output $ parseErrorPretty err
    Right la -> hPutStr output $ show la
