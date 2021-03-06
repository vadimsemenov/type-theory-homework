{-# LANGUAGE OverloadedStrings #-}

module FreeVariablesSpec
       ( spec
       ) where

import           SpecUtil
import           Test.Hspec

import           LambdaCalculus
import           Parser

import           Data.Either    (fromRight)
import qualified Data.Set       as Set (fromList, empty)
import qualified Data.Text      as T


spec :: Spec
spec = do
    describe "1st hw sample" $ do
        let input = "\\a.\\b.a b c (\\d.e \\f.g) h"
        let expected = Set.fromList ["c", "e", "g", "h"]
        it (T.unpack input) $
            freeVariables (parse input) `shouldBe` expected
    describe "Y-combinator" $ do
        let ycombinator = "\\f.(\\x.f (x x)) (\\x.f (x x))"
        it "shouldn't have free variables" $
            freeVariables (parse ycombinator) `shouldBe` Set.empty
    describe "random tests" $ do
        let fst = "\\x.c a x \\a.b a x"
        let fstExpected = Set.fromList ["a", "b", "c"]
        let snd = "a b c \\a.a"
        let sndExpected = Set.fromList ["a", "b", "c"]
        it (T.unpack fst) $
            freeVariables (parse fst) `shouldBe` fstExpected
        it (T.unpack snd) $
            freeVariables (parse snd) `shouldBe` sndExpected
