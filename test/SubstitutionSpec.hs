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
        let snd = "\\x.f x  [y := y x]"
        let sndExpected = "\\x.f x"
        let trd = "y x [y := y']"
        let trdExpected = "y' x"
        it (T.unpack fst) $
            substitute (parseSubstitution fst) `shouldBe` Right (parse fstExpected)
        it (T.unpack snd) $
            substitute (parseSubstitution snd) `shouldBe` Right (parse sndExpected)
        it (T.unpack trd) $
            substitute (parseSubstitution trd) `shouldBe` Right (parse trdExpected)
    describe "impossible" $ do
        let fst = "\\f.f a [a := f a]"
        it (T.unpack fst) $
            substitute (parseSubstitution fst) `shouldBe` Left "f"
