{-# LANGUAGE OverloadedStrings #-}

module SubstitutionSpec
       ( spec
       ) where

import           SpecUtil
import           Test.Hspec

import           LambdaCalculus
import           Parser

import           Data.Either    (fromRight, isLeft)
import qualified Data.Text      as T


spec :: Spec
spec = do
    describe "possible" $ do
        let fst = "a b \\x.\\f.f a x b[a := \\f.f a]"
        let fstExpected = "(\\f.f a) b \\x.\\f.f (\\f.f a) x b"
        it (T.unpack fst) $
            substitute (parseSubstitution fst) `shouldBe` Right (parse fstExpected)
    describe "impossible" $ do
        let fst = "\\f.f a [a := f a]"
        it (T.unpack fst) $
            substitute (parseSubstitution fst) `shouldBe` Left "f"
