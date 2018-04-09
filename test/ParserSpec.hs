{-# LANGUAGE OverloadedStrings #-}

module ParserSpec
       ( spec
       ) where

import           Test.Hspec

import           LambdaCalculus
import           Parser

import           Data.Either    (fromRight, isLeft)
import qualified Data.Text      as T


spec :: Spec
spec = do
    describe "1st hw sample" $ do
        let input = "\\a.\\b.a b c (\\d.e \\f.g) h"
        let expected = "(\\a.(\\b.((((a b) c) (\\d.(e (\\f.g)))) h)))"
        it "parsed" $
            parseLambda input `shouldBe` (Right $
                Abstraction "a" (Abstraction "b"
                (Application
                (Application
                (Application
                (Application (Variable "a") (Variable "b"))
                (Variable "c"))
                (Abstraction "d"
                (Application (Variable "e")
                             (Abstraction "f" (Variable "g")))))
                (Variable "h"))
                ))
        it "shown" $
            showWithParenthesis (fromRight (Variable "impossible") (parseLambda input)) `shouldBe` expected
    describe "whitespaces" $ do
        context "id" $ do
            let expected = Right $ Abstraction "x" (Variable "x")
            let simple = "\\x.x"
            let trim = " \t\\x.x"
            let spacesEverywhere = " \t \\  x \n .\tx\t"
            it (T.unpack simple) $
                parseLambda simple `shouldBe` expected
            it (T.unpack trim) $
                parseLambda trim `shouldBe` expected
            it (T.unpack spacesEverywhere) $
                parseLambda spacesEverywhere `shouldBe` expected
        context "application" $ do
            let fx = "f x"
            let fgx = "f g x"
            let fogx = "f (g x)"
            let idw = "(\\x.x) w"
            let lxw = "\\x.x w"
            let aid = "a \\x.x"
            let aid' = "a\\x.x"
            it (T.unpack fx) $
                parseLambda fx `shouldBe` (Right $ Application (Variable "f") (Variable "x"))
            it (T.unpack fgx) $
                parseLambda fgx `shouldBe` (Right $
                    Application (Application (Variable "f") (Variable "g"))
                                (Variable "x"))
            it (T.unpack fogx) $
                parseLambda fogx `shouldBe` (Right $
                    Application (Variable "f")
                                (Application (Variable "g") (Variable "x")))
            it (T.unpack idw) $
                parseLambda idw `shouldBe` (Right $
                    Application (Abstraction "x" (Variable "x"))
                                (Variable "w"))
            it (T.unpack lxw) $
                parseLambda lxw `shouldBe` (Right $
                    Abstraction "x" (Application (Variable "x") (Variable "w")))
            it (T.unpack aid) $
                parseLambda aid `shouldBe` (Right $
                    Application (Variable "a")
                                (Abstraction "x" (Variable "x")))
            it (T.unpack aid') $
                parseLambda aid' `shouldSatisfy` isLeft
