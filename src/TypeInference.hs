{-# LANGUAGE OverloadedStrings #-}

module TypeInference
       ( infer
       , unifyLambda
       , createSystem
       , showType
       , showTypedTerm
       ) where

import           LambdaCalculus
import qualified Unification         as U

import           Control.Monad.State
import           Data.List           (find)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Text           as T


-- data TType = TName Literal | TArrow TType TType
type TType = U.Term

type TypedTerm = (Literal, TType)
type Context = [TypedTerm]

showType :: TType -> String
showType (U.Variable name) = T.unpack name
showType (U.Function "arrow" [lhs, rhs]) = "(" ++ showType lhs ++ "->" ++ showType rhs ++ ")"

showTypedTerm :: TypedTerm -> String
showTypedTerm (var, varType) = T.unpack var ++ " : " ++ showType varType

type VarTypeMapping = Map Literal TType
type Namespace = Set Literal
type BoundedVariables = Set Literal

atomic :: Literal -> TType
atomic = U.Variable

arrow :: TType -> TType -> TType
arrow lhs rhs = U.Function "arrow" [lhs, rhs]

type InferenceContext = (VarTypeMapping, Namespace, BoundedVariables, U.EquationSystem)

getType :: Literal -> State InferenceContext TType
getType var = gets $ \(vm, _, _, _) -> vm Map.! var

addBoundVariable :: Literal -> State InferenceContext (TType, Maybe TType)
addBoundVariable var = do
    maybePrev <- gets $ \(mapping, _, _, _) -> Map.lookup var mapping
    typeName <- findNewTypeName var
    let varType = atomic typeName
    modify $ \(mapping, namespace, bounded, equations) ->
        (Map.insert var varType mapping, Set.insert typeName namespace, Set.insert var bounded, equations)
    return (varType, maybePrev)

findNewTypeName :: Literal -> State InferenceContext Literal
findNewTypeName var = (gets $ \(_, namespace, _, _) -> Set.member var namespace) >>= \member ->
    if member then findNewTypeName (T.snoc var '\'') else return var

isBound :: Literal -> State InferenceContext Bool
isBound var = gets $ \(_, _, bv, _) -> Set.member var bv

appendEquation :: U.Equation -> State InferenceContext ()
appendEquation equation = modify $ \(vm, ns, bv, es) -> (vm, ns, bv, equation : es)

createSystem :: Lambda -> (TType, InferenceContext)
createSystem lambda = runState (walk lambda) (fvMapping, fv, Set.empty, [])
  where
    fv = freeVariables lambda
    fvMapping = Map.fromAscList $ map (\var -> (var, atomic var)) $ Set.toAscList fv
    fourth (_, _, _, f) = f

walk :: Lambda -> State InferenceContext TType
walk (Variable name) = getType name
walk (Abstraction over body) = do
    (overType, maybePrev) <- addBoundVariable over
    bodyType <- walk body
    case maybePrev of
        Just prevType -> modify $ \(mapping, ns, bv, es) -> (Map.insert over prevType mapping, ns, bv, es)
        Nothing       -> modify $ \(mapping, ns, bv, es) -> (Map.delete over mapping, ns, Set.delete over bv, es)
    return $ arrow overType bodyType
walk (Application lhs rhs) = do
    lhsType <- walk lhs
    rhsType <- walk rhs
    retType <- findNewTypeName (T.pack "t")
    modify $ \(vm, namespace, bv, es) -> (vm, Set.insert retType namespace, bv, es)
    let value = atomic retType
    appendEquation (lhsType U.:=: arrow rhsType (atomic retType))
    return value

unifyLambda :: Lambda -> Maybe U.EquationSystem
unifyLambda = undefined -- U.unify . createSystem

infer :: Lambda -> Maybe (TType, Context)
infer lambda = U.unify system >>= \solution -> let context = toContext solution
                                               -- in return (((Map.fromList context) Map.! lambdaType), context)
                                               in return (lambdaType, context)
  where
    -- ((U.Variable lambdaType), (mapping, _, _, system)) = createSystem lambda
    (lambdaType, (mapping, _, _, system)) = createSystem lambda
    toContext []                                  = []
    toContext ((U.Variable var U.:=: lhs) : rest) = (var, lhs) : toContext rest
