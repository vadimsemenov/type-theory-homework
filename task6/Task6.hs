{-# LANGUAGE LambdaCase #-}

module Main where

import           TypeInference (infer, showType, showTypedTerm)
import           Parser
import           Util

import           Text.Megaparsec.Error (parseErrorPretty)

import           Data.List             (sort)
import qualified Data.Text             as T1
import qualified Data.Text.IO          as T
import           System.IO


main :: IO ()
main = io $ \input output -> parseLambda <$> T.hGetContents input >>= \case
    Left err -> hPutStr output $ parseErrorPretty err
    Right la -> hPutStr output $ prettyShow $ infer la

prettyShow Nothing = "Лямбда-выражение не имеет типа."
prettyShow (Just (lambdaType, solution)) = showType lambdaType ++ "\n" ++ unlines (map showTypedTerm solution)
