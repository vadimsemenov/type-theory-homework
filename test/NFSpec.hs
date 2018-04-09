{-# LANGUAGE OverloadedStrings #-}

module NFSpec
       ( spec
       ) where

import           SpecUtil
import           Test.Hspec

import           LambdaCalculus
import           Normalization  (normalize)
import           Parser

import           Data.Either    (fromRight)
import qualified Data.Text      as T


spec :: Spec
spec = do
    describe "normalized" $ do
        let fst = "a b c d e f g"
        let snd = "f g \\x. f g \\y.x y"
        it (T.unpack fst) $ do
            let l = parse fst
            normalize l `shouldBe` l
        it (T.unpack snd) $ do
            let l = parse snd
            normalize l `shouldBe` l
    describe "simple" $ do
        let fst  = "f x ((\\x.f x) (\\x.x))"
        let fst' = "f x (f (\\x.x))"
        let snd  = "f x ((\\x.\\y.y x) (\\z.z y))"
        let snd' = "f x (\\y'.y' (\\z.z y))"
        let trd  = "y' x ((\\x.\\y.y x) (\\z.z y)) y''"
        let trd' = "y' x (\\y'''.y''' (\\z.z y)) y''"
        let frt  = "f x ((\\x.\\z.\\y.y x) (\\z.z y))"
        let frt' = "f x (\\z.\\y'.y' (\\z.z y))"
        let fvt  = "(\\f.f \\y. \\x.f x) (x)"
        let fvt' = "(x \\y.\\x'.x x')"
        let six  = "(\\f.f x) \\x.x"
        let six' = "x"
        let snt  = "(\\f.f \\x.x) \\x.x"
        let snt' = "\\x.x"
        let egh  = "(\\f.f x) (x)"
        let egh' = "x x"
        let nth  = "(\\y.\\f.f y x) (f)"
        let nth' = "\\f'.f' f x"
        it (T.unpack fst) $
            normalize (parse fst) `shouldBe` parse fst'
        it (T.unpack snd) $
            normalize (parse snd) `shouldBe` parse snd'
        it (T.unpack trd) $
            normalize (parse trd) `shouldBe` parse trd'
        it (T.unpack frt) $
            normalize (parse frt) `shouldBe` parse frt'
        it (T.unpack fvt) $
            normalize (parse fvt) `shouldBe` parse fvt'
        it (T.unpack six) $
            normalize (parse six) `shouldBe` parse six'
        it (T.unpack snt) $
            normalize (parse snt) `shouldBe` parse snt'
        it (T.unpack egh) $
            normalize (parse egh) `shouldBe` parse egh'
        it (T.unpack nth) $
            normalize (parse nth) `shouldBe` parse nth'
    describe "candy for Vlad" $ do
        let t1 = "(\\x.x) (\\y.x)"
        let t2 = "(\\y.x) (\\x.x)"
        it (T.unpack t1) $
            normalize (parse t1) `shouldBe` parse "(\\y.x)"
        it (T.unpack t2) $
            normalize (parse t2) `shouldBe` parse "x"
