{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module W
  ( getType
  ) where


  import Control.Monad.Except
  import Control.Monad.State
  import Data.List ((\\), union)
  import Simplifier (Expression (..))
  import Resolver (resolveSystem, insertType, getVars, getFreeVars, TypeExpr (..), Matcher)
  import qualified Data.Map.Strict as Map

  data TypeError = VariableNotInContext String
                 | SystemUnresolvable
                 deriving (Show)

  newtype WState m a = WState { runWState :: StateT Integer (ExceptT TypeError m) a }
    deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState Integer)

  getType :: Monad m => Expression -> m (Either TypeError (Matcher, TypeExpr))
  getType e = do
    result <- runExceptT $ runStateT (runWState $ getWState Map.empty e) 0
    return (result >>= return . fst)

  getWState :: ( MonadState Integer           m
               , MonadError         TypeError m
               ) => Matcher -> Expression -> m (Matcher, TypeExpr)
  getWState c (Application l r) = do
    (ctxL, typeL) <- getWState c l
    (ctxR, typeR) <- getWState (fmap (insertType ctxL) c) r

    index <- get
    modify (+1)
    let var = TypeVariable $ getTypeVar index
    let eq = Equation (insertType ctxR typeL) (Implication typeR var)
    case resolveSystem [eq] of
      Just x  -> do
        let ss = Map.union (fmap (insertType ctxL) ctxR) ctxL
        let m = Map.union (fmap (insertType x) ss) x
        return (m, insertType m var)
      Nothing -> throwError $ SystemUnresolvable
  getWState c (Lambda    s e  ) = do
    index <- get
    modify (+1)
    let var = TypeVariable $ getTypeVar index

    (ctxE, typeE) <- getWState (Map.insert s var c) e
    return (ctxE, Implication (insertType ctxE var) typeE)
  getWState c (Let       s e i) = do
    (ctxE, typeE) <- getWState c e
    let ctxM = fmap (insertType ctxE) (Map.delete s c)
    let newM = insertType ctxE (quantifiering typeE c)

    (ctxI, typeI) <- getWState (Map.insert s newM ctxM) i
    return (Map.union (fmap (insertType ctxI) ctxE) ctxI, typeI)
  getWState c (Variable  s    ) = do
    case Map.lookup s c of
      Just x  -> do
        let vars = (getVars x) \\ (getFreeVars x)
        let uX = unwrap x
        index <- get
        let m = byList index vars
        modify (+ (toInteger $ length vars))
        return (Map.empty, insertType m uX)
      Nothing -> throwError $ VariableNotInContext s

  getTypeVar :: Integer -> String
  getTypeVar i = "a" ++ show i

  byList :: Integer -> [String] -> Matcher
  byList i (x:xs) = Map.insert x (TypeVariable $ getTypeVar i) (byList (i + 1) xs)
  byList _ []     = Map.empty

  unwrap :: TypeExpr -> TypeExpr
  unwrap (UniversalQ _ e) = unwrap e
  unwrap e                = e

  quantifiering :: TypeExpr -> Matcher -> TypeExpr
  quantifiering e m = do
    let ctxFree = foldl union [] (fmap getFreeVars (Map.elems m))
    let frees = (getFreeVars e) \\ ctxFree
    quantifieringR frees e
      where
        quantifieringR :: [String] -> TypeExpr -> TypeExpr
        quantifieringR (x:xs) t = UniversalQ x (quantifieringR xs t)
        quantifieringR []     t = t
