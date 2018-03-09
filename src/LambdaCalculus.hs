{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaCalculus where

import qualified Data.Text as T


{-
<Expression>  ::= [<Application>] '\' <Variable> '.' <Expression>
                | <Application>
<Application> ::= <Application> <Atom> | <Atom>
<Atom>        ::= '(' <Expression> ')' | <Variable>
<Variable>    ::= ('a'...'z') {'a'...'z'|'0'...'9'|'''}*
-}

type Literal = T.Text

data LambdaG a = Application (LambdaG a) (LambdaG a)
               | Abstraction a (LambdaG a)
               | Variable a

type Lambda = LambdaG Literal

data LambdaWithSubstitution = LambdaWithSubstitution
             { into :: Lambda
             , var  :: Literal
             , what :: Lambda
             }

deriving instance Eq Lambda

instance Show Lambda where
    show = showWithParenthesis

instance Show LambdaWithSubstitution where
  show ls = showShort (into ls) ++ "[" ++ T.unpack (var ls) ++ " := " ++ showShort (what ls) ++ "]"

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


freeVariables :: Lambda -> [Literal]
freeVariables (Variable v) = [v]
freeVariables (Abstraction x xs) = filter (/= x) $ freeVariables xs
freeVariables (Application lhs rhs) = freeVariables lhs `merge` freeVariables rhs
  where
    merge :: [Literal] -> [Literal] -> [Literal]
    merge [] fv = fv
    merge fv [] = fv
    merge (l : ls) (r : rs)
        | l < r     = l : merge ls (r : rs)
        | l > r     = r : merge (l : ls) rs
        | otherwise = l : merge ls rs

substitute :: LambdaWithSubstitution -> Either Literal Lambda
substitute (LambdaWithSubstitution into instead what) = substitute' into (freeVariables what)
  where
    substitute' was@(Variable x) fv = if x == instead then Right what else Right was
    substitute' (Application lhs rhs) fv = Application <$> substitute' lhs fv <*> substitute' rhs fv
    substitute' was@(Abstraction x xs) fv
        | x == instead = Right was
        | x `elem` fv  = Left x
        | otherwise    = Abstraction x <$> substitute' xs fv
