module Unification
       ( Equation (..)
       , Term (..)
       , FunctionLiteral
       , VariableLiteral
       , EquationSystem
       , unify
       ) where


import           Control.Monad (join)
import qualified Data.Text     as T
import           Debug.Trace   (traceShowId)


data Equation = Term :=: Term
    deriving (Show, Eq)
infixl 8 :=:

data Term = Function FunctionLiteral [Term] | Variable VariableLiteral
    deriving (Show, Eq)

type Literal = T.Text
type FunctionLiteral = Literal
type VariableLiteral = Literal

type EquationSystem = [Equation]

{-
Curry-Howard Isomorphism by Morten Heine B. Sorensen and Pawel Urzyczyn (page 94)

a) Replace "x = t" and "x = s" (where t is not a variable) by "x = t" and "t = s";
b) Replace "t = x" by "x = t";
c) Replace "f(t1, ..., tn) = f(u1, ..., un)" by "t1 = u1", ..., "tn = un";
d) Replace "x = t" and "r = s" by "x = t" and "r[x := t] = s[x := t]";
e) Remove an equation of the form "t = t".
-- -}

unify :: EquationSystem -> Maybe EquationSystem
unify system = unificationStep system >>= \system' ->
    if system == system' then Just system else unify system'

unificationStep :: EquationSystem -> Maybe EquationSystem
unificationStep system = swapAll system >>=
                         checkValid >>=
                         filterLeftAll >>=
                         unrollAll >>=
                         substituteAll >>=
                         filterEqualsAll

-- Check absence of "x = t" where t contains x
checkValid :: EquationSystem -> Maybe EquationSystem
checkValid = traverse checkValid'
  where
    checkValid' eq@(Variable var :=: f@(Function _ _)) = if member var f then Nothing else Just eq
    checkValid' eq@(f@(Function _ _) :=: Variable var) = if member var f then Nothing else Just eq
    checkValid' eq                                       = Just eq

    member var (Variable var')   = var == var'
    member var (Function _ args) = any (member var) args

-- a)
filterLeftAll :: EquationSystem -> Maybe EquationSystem
filterLeftAll system = Just $ go system
  where
    go :: EquationSystem -> EquationSystem
    go []      = []
    go (h : t) = h : go (filterLeft h t)

filterLeft :: Equation -> EquationSystem -> EquationSystem
filterLeft (Variable var :=: t) system = map replaceLeft system
  where
    replaceLeft :: Equation -> Equation
    replaceLeft (v@(Variable var') :=: s) = (if var' == var then t else v) :=: s
    replaceLeft eq                        = eq
filterLeft _ system = system

-- b)
swapAll :: EquationSystem -> Maybe EquationSystem
swapAll system = Just $ map swapIfNeeded system

swapIfNeeded :: Equation -> Equation
swapIfNeeded (f@(Function _ _) :=: v@(Variable _)) = v :=: f
swapIfNeeded eq                                    = eq

-- c)
unrollAll :: EquationSystem -> Maybe EquationSystem
unrollAll system = join <$> traverse unrollFunction system

unrollFunction :: Equation -> Maybe EquationSystem
unrollFunction (Function f fArgs :=: Function g gArgs) =
    if f == g && length fArgs == length gArgs
    then Just $ zipWith (:=:) fArgs gArgs
    else Nothing
unrollFunction eq = Just [eq]

-- d)
substituteAll :: EquationSystem -> Maybe EquationSystem
substituteAll = substitute' []
    where
      substitute' pred [] = Just $ reverse pred
      substitute' pred (eq@(Variable var :=: t) : rest) = if lhs == pred && rhs == rest
                                                            then substitute' (eq : pred) rest
                                                            else Just $ reverse lhs ++ (eq : rhs)
        where
          substitution = (var, t)
          (lhs, rhs) = (substituteEverywhere substitution pred, substituteEverywhere substitution rest)

substituteEverywhere :: (VariableLiteral, Term) -> EquationSystem -> EquationSystem
substituteEverywhere substitution = map (substituteEquation substitution)

substituteEquation :: (VariableLiteral, Term) -> Equation -> Equation
substituteEquation substitution (lhs :=: rhs) = substitute substitution lhs :=: substitute substitution rhs

substitute :: (VariableLiteral, Term) -> Term -> Term
substitute (instead, what) = substitute'
  where
    substitute' var@(Variable name)      = if name == instead then what else var
    substitute' fun@(Function name args) = Function name $ map substitute' args

-- e)
filterEqualsAll :: EquationSystem -> Maybe EquationSystem
filterEqualsAll system = Just $ filter equalityFilter system

equalityFilter :: Equation -> Bool
equalityFilter (lhs :=: rhs) = lhs /= rhs

