{-# LANGUAGE OverloadedStrings #-}

module SpecUtil where

import           LambdaCalculus
import           Parser

import           Data.Either    (fromRight)
import qualified Data.Text      as T


fiasco :: Lambda
fiasco = Variable "fiasco"

fiascoSubstitution :: LambdaWithSubstitution
fiascoSubstitution = LambdaWithSubstitution fiasco "fiasco" fiasco

parse :: T.Text -> Lambda
parse = fromRight fiasco . parseLambda

parseSubstitution :: T.Text -> LambdaWithSubstitution
parseSubstitution = fromRight fiascoSubstitution . parseLambdaWithSubstitution
