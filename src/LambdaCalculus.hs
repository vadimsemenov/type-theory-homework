{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaCalculus where

import qualified Data.Text as T


type Literal = T.Text

data LambdaG a = Application (LambdaG a) (LambdaG a)
               | Abstraction a (LambdaG a)
               | Variable a

type Lambda = LambdaG Literal

deriving instance Eq Lambda

instance Show Lambda where
    show = showWithParenthesis


showWithParenthesis :: Lambda -> String
-- showWithParenthesis (Application lhs rhs@(Application _ _)) = "(" ++ showWithParenthesis lhs ++  " (" ++ showWithParenthesis rhs ++ "))"
showWithParenthesis (Application lhs rhs) = "(" ++ showWithParenthesis lhs ++  " " ++ showWithParenthesis rhs ++ ")"
-- showWithParenthesis (Application lhs rhs) = showWithParenthesis lhs ++  " " ++ showWithParenthesis rhs
showWithParenthesis (Abstraction x xs) = "(\\" ++ T.unpack x ++ "." ++ showWithParenthesis xs ++ ")"
showWithParenthesis (Variable v) = T.unpack v

showShort :: Lambda -> String
showShort (Application lhs rhs@(Application _ _)) = showShort lhs ++  " (" ++ showShort rhs ++ ")"
showShort (Application lhs rhs) = showShort lhs ++ " " ++ showShort rhs
showShort (Abstraction x xs) = "(\\" ++ T.unpack x ++ "." ++ showShort xs ++ ")"
showShort (Variable v) = T.unpack v
