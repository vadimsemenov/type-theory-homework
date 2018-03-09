{-# LANGUAGE LambdaCase #-}

module Main where

import           LambdaCalculus
import           Parser
import           Util

import           Text.Megaparsec.Error (parseErrorPretty)

import           Data.List             (sort)
import qualified Data.Text             as T1
import qualified Data.Text.IO          as T
import           System.IO


main :: IO ()
main = io $ \input output -> parseLambdaWithSubstitution <$> T.hGetContents input >>= \case
    Left err -> hPutStr output $ parseErrorPretty err
    Right su -> hPutStr output $ case substitute su of
        Left lit -> "Нет свободы для подстановки для переменной " ++ T1.unpack lit
        Right la -> show la
