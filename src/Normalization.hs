{-# LANGUAGE LambdaCase #-}

module Normalization
       ( normalize
       ) where


import           Debug.Trace         (traceM, traceShowId)

import           Control.Monad.State
import           Data.Foldable       (for_)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (snoc)
import           LambdaCalculus


normalize :: Lambda -> Lambda
normalize lambda = toLambda $ evalState (repeatNf $ fromLambda lambda) (Map.empty, namespaceOf lambda)


data Lambda' = Var Literal
             | App Bool Lambda' Lambda'
             | Abs Bool Literal Lambda'
  deriving (Show, Eq)

type SubstitutionMap = Map Literal Lambda'
type Namespace = Set Literal

type NormalizationState a = State (SubstitutionMap, Namespace) a

getSubstitutionMap :: NormalizationState SubstitutionMap
getSubstitutionMap = gets fst

getNamespace :: NormalizationState Namespace
getNamespace = gets snd

isNormalized :: Lambda' -> NormalizationState Bool
isNormalized (Var name)           = state $ \s@(sm, _) -> (not $ Map.member name sm, s)
isNormalized (App normalized _ _) = state $ \s -> (normalized, s)
isNormalized (Abs normalized _ _) = state $ \s -> (normalized, s)

inNamespace :: Literal -> NormalizationState Bool
inNamespace name = state $ \s@(_, ns) -> (Set.member name ns, s)

addToNamespace :: Literal -> NormalizationState ()
addToNamespace name = state $ \(sm, ns) -> ((), (sm, Set.insert name ns))

getSubstitution :: Literal -> NormalizationState (Maybe Lambda')
getSubstitution name = state $ \s@(sm, _) -> (Map.lookup name sm, s)

addSubstitution :: Literal -> Lambda' -> NormalizationState ()
addSubstitution name substitution = state $ \(sm, ns) -> ((), (Map.insert name substitution sm, ns))

namespaceOf :: Lambda -> Set Literal
namespaceOf (Variable v)          = Set.singleton v
namespaceOf (Abstraction v vBody) = Set.insert v $ namespaceOf vBody
namespaceOf (Application lhs rhs) = Set.union (namespaceOf lhs) (namespaceOf rhs)

fromLambda :: Lambda -> Lambda'
fromLambda (Variable v)          = Var v
fromLambda (Abstraction v vBody) = Abs False v $ fromLambda vBody
fromLambda (Application lhs rhs) = App False (fromLambda lhs) (fromLambda rhs)

toLambda :: Lambda' -> Lambda
toLambda (Var v)            = Variable v
toLambda (Abs True v vBody) = Abstraction v (toLambda vBody)
toLambda (App True lhs rhs) = Application (toLambda lhs) (toLambda rhs)
toLambda lambda = error $ "Not normalized lambda expression: " ++ show lambda

repeatNf :: Lambda' -> NormalizationState Lambda'
repeatNf lambda = do
    norm <- isNormalized lambda
    if norm then return lambda
    else nf lambda >>= repeatNf

nf :: Lambda' -> NormalizationState Lambda'
nf var@(Var v)                          =
    getSubstitution v >>= \case
        Nothing           -> return var
        Just substitution -> do
            substitution' <- nf substitution
            addSubstitution v substitution'
            norm <- isNormalized substitution'
            return $ if norm then substitution'
                     else var
nf abs@(Abs normalized v body)          =
    if normalized then
        return abs
    else do
        body'       <- nf body
        normalized' <- isNormalized body'
        return $ Abs normalized' v body'
nf app@(App normalized lhs rhs)         =
    if normalized then
        return app
    else do
        normLhs <- isNormalized lhs
        normRhs <- isNormalized rhs
        if normLhs && normRhs then
            return $ App True lhs rhs
        else if normLhs then do
            rhs'     <- nf rhs
            normRhs' <- isNormalized rhs'
            return $ App normRhs' lhs rhs'
        else tryExtractLambda lhs >>= \case
            Just lambda ->
                reduce $ App False lambda rhs
            Nothing     -> do
                lhs'     <- nf lhs
                normLhs' <- isNormalized lhs'
                return $ App (normLhs' && normRhs) lhs' rhs

tryExtractLambda :: Lambda' -> NormalizationState (Maybe Lambda')
tryExtractLambda abs@Abs {}     = return $ Just abs
tryExtractLambda var@(Var name) = getSubstitution name >>= \case
    Nothing     -> return Nothing
    Just lambda -> tryExtractLambda lambda
tryExtractLambda _              = return Nothing

reduce :: Lambda' -> NormalizationState Lambda'
reduce (App _ (Abs _ x body) substitution) = do
    boundedInBody <- bv x body
    freeInSubstitution <- fv substitution
    let toRename = Set.intersection boundedInBody freeInSubstitution
    (x', body') <- rename x body toRename
    addSubstitution x' substitution
    return body'
reduce l = error $ "Cannot reduce non-redex: " ++ show l

rename :: Literal -> Lambda' -> Set Literal -> NormalizationState (Literal, Lambda')
rename x body toRename = do
    renamings <- traverse newName $ Map.fromAscList $
                 map (\name -> (name, name)) $
                 Set.toAscList $ Set.insert x toRename
    let x' = renamings Map.! x
    substitutionMap <- getSubstitutionMap
    let body' = evalState (rename' body substitutionMap) (Map.insert x x' renamings)
    return (x', body')
      where
        newName :: Literal -> NormalizationState Literal
        newName name = do
            used <- inNamespace name
            if used then newName $ snoc name '\'' else do
                addToNamespace name
                return name

        rename' :: Lambda' -> SubstitutionMap -> State (Map Literal Literal) Lambda'
        rename' (App normalized lhs rhs)     sm = App normalized <$> rename' lhs sm <*> rename' rhs sm
        rename' abs@(Abs normalized v vBody) sm = do
            assertSubstitution v vBody sm
            if v == x then
                renameFree abs sm
            else do
                v' <- gets (Map.findWithDefault v v)
                vBody' <- rename' vBody sm
                return $ Abs normalized v' vBody'
        rename' (Var v) sm =
            if Map.member v sm then
                -- TODO: maybe it's possible to normalize substitution and memoize it
                rename' (sm Map.! v) sm
            else do
                v' <- gets (Map.findWithDefault v v)
                return $ Var v'

        renameFree :: Lambda' -> SubstitutionMap -> State (Map Literal Literal) Lambda'
        renameFree (App normalized lhs rhs) sm = App normalized <$> renameFree lhs sm <*> renameFree rhs sm
        renameFree (Abs normalized v vBody) sm = do
            assertSubstitution v vBody sm
            vMapping <- state $ Map.updateLookupWithKey (\_ _ -> Nothing) v
            vBody' <- renameFree vBody sm
            for_ vMapping $ \v' ->
                modify (Map.insert v v')
            return $ Abs normalized v vBody'
        renameFree (Var v) sm =
            if Map.member v sm then
                renameFree (sm Map.! v) sm
            else do
                v' <- gets (Map.findWithDefault v v)
                return $ Var v'

        assertSubstitution :: Literal -> Lambda' -> SubstitutionMap -> State (Map Literal Literal) ()
        assertSubstitution v vBody sm =
            for_ (Map.lookup v sm) $ \substitution ->
                error $ "Bounded substitution! (\\" ++ show v ++ "." ++ show vBody ++ ")" ++
                        "[" ++ show v ++ ":=" ++ show substitution ++ "]"

fv :: Lambda' -> NormalizationState (Set Literal)
fv (App _ lhs rhs)   = Set.union <$> fv lhs <*> fv rhs
fv (Abs _ name body) = Set.delete name <$> fv body
fv (Var v)           = (Map.lookup v <$> getSubstitutionMap) >>= \case
    Just substitution -> fv substitution
    Nothing           -> return $ Set.singleton v

bv :: Literal -> Lambda' -> NormalizationState (Set Literal)
bv name lambda = bv' lambda Set.empty
  where
    bv' (App _ lhs rhs) bounded = Set.union <$> bv' lhs bounded <*> bv' rhs bounded
    bv' (Abs _ x body)  bounded = if x == name then return Set.empty
                                  else bv' body (Set.insert x bounded)
    bv' (Var v)         bounded = if v == name then return bounded
                                  else (Map.lookup v <$> getSubstitutionMap) >>= \case
                                      Just substitution -> bv' substitution bounded
                                      Nothing           -> return Set.empty
