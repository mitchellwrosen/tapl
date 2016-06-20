{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Simply typed lambda calculus extended with base types, unit, sequencing,
-- ascription, let bindings, tuples, variants, fixpoint, and mutable references.

module ExtendedSimplyTypedLambdaCalc where

import Utils

import Control.Exception    (assert)
import Control.Monad        (forM)
import Control.Monad.Except
import Control.Monad.State
import Data.IntMap.Strict   (IntMap)
import Data.Monoid
import Data.Set             (Set)
import Data.Text            (Text)

import qualified Data.IntMap as IntMap
import qualified Data.Set    as Set
import qualified Data.Text   as Text

-- t ::=                    -- terms:
--       x                  -- variable
--       \x:T.t             -- abstraction
--       t t                -- application
--       t as T             -- ascription
--       t;t                -- sequence
--       let x=t in t       -- let binding
--       {t,...}            -- tuple
--       t.i                -- tuple projection
--       {l=t,...}          -- record
--       t.l                -- record projection
--       <l=t> as T         -- tagging
--       case t of <l=x>=>t -- case
--       fix t              -- fixed point of t
--       ref t              -- reference creation
--       !t                 -- dereference
--       t:=t               -- assignment
--       l                  -- store location
--       unit               -- constant unit
--
-- v ::=                    -- values:
--       \x:T.t             -- abstraction value
--       {v,...}            -- tuple value
--       {l=v,...}          -- record value
--       <l=v> as T         -- variant value
--       l                  -- store location
--       unit               -- unit value
--
--
-- T ::=                    -- types:
--       T -> T             -- type of functions
--       {T,...}            -- type of tuples
--       {l:T,...}          -- type of records
--       <l:T...>           -- type of variants
--       Ref T              -- type of reference cells
--       Unit               -- unit type
--       Base               -- base type (uninterpreted)
--
-- μ ::=                    -- stores:
--       ∅                  -- empty store
--       μ,l=v              -- location binding
--
--
-- Γ ::=                    -- contexts:
--       ∅                  -- empty context
--       Γ,x:T              -- term variable binding
--
-- Σ ::=                    -- store typings:
--       ∅                  -- empty store typing
--       Σ,l:T              -- location typing

data Term
  = Var Int
  | Lam Hint Type Term
  | App Term Term
  | Asc Term Type
  | Seq Term Term
  | Let Hint Term Term
  | Tuple [Term]
  | PrjT Term Int
  | Record [(Label,Term)]
  | PrjR Term Label
  | Inj Label Term Type
  | Case Term [(Label,Hint,Term)]
  | Fix Term
  | Ref Term
  | Deref Term
  | Assign Term Term
  | Location Location
  | Unit
  deriving (Show)

-- A hint about what to name the bound variable when pretty printing
type Hint = Text

type Label = Text

-- A key ("location") into the mutable store.
type Location = Int


-- Mutable storage (grows indefinitely; no GC)
data Store
  = Store
      !Location     -- ^ Next available location
      (IntMap Term) -- ^ Storage

-- Store a new value, returning its new location.
storeNewValue :: MonadState Store m => Term -> m Location
storeNewValue t = do
  assertM (isval t)
  Store l s <- get
  put (Store (l+1) (IntMap.insert l t s))
  pure l

-- Overwrite an existing location with a value.
storeValue :: MonadState Store m => Location -> Term -> m ()
storeValue l t = do
  assertM (isval t)
  Store l0 s <- get
  assertM (IntMap.member l s)
  put (Store l0 (IntMap.insert l t s))


lookupValue :: MonadState Store m => Location -> m Term
lookupValue l = do
  Store _ s <- get
  case IntMap.lookup l s of
    Nothing -> error ("lookupVal: location " ++ show l ++ " not in store " ++ show s)
    Just t  -> pure t


data Type
  = TyArr Type Type
  | TyTuple [Type]
  | TyRecord [(Label,Type)]
  | TyVariant [(Label,Type)]
  | TyRef Type
  | TyUnit
  | TyBase Base
  deriving (Eq, Show)

data Base
  = Bool
  | Nat
  deriving (Eq, Show)

data Subst = Subst !Int Term

type Ctx = [Type]


type StoreTypings = IntMap Type

lookupType :: MonadState StoreTypings m => Location -> m Type
lookupType l = do
  s <- get
  case IntMap.lookup l s of
    Nothing -> error ("lookupType: location " ++ show l ++ " not in store typings " ++ show s)
    Just ty -> pure ty


type TypeError = Text


typeof :: Ctx -> Term -> StateT StoreTypings (Except TypeError) Type
typeof c = \case
  Var n -> pure (c !! n)

  Lam _ ty1 t -> do
    ty2 <- typeof (ty1:c) t
    pure (TyArr ty1 ty2)

  App t1 t2 -> do
    typeof c t1 >>= \case
      TyArr ty1 ty2 -> do
        ty1' <- typeof c t2
        unless (ty1 == ty1')
          (throwError ("Cannot apply function of type "
                         <> tshow (TyArr ty1 ty2)
                         <> " to value of type "
                         <> tshow ty1'))
        pure ty2
      ty1 -> throwError ("Cannot apply type " <> tshow ty1 <> " to a value")

  Asc t ty -> do
    ty' <- typeof c t
    unless (ty == ty')
      (throwError ("Cannot ascribe type "
                     <> tshow ty
                     <> " to value of type "
                     <> tshow ty'))
    pure ty

  Seq t1 t2 ->
    typeof c t1 >>= \case
      TyUnit -> typeof c t2
      ty1 -> throwError ("Cannot sequence value of type " <> tshow ty1)

  Let _ t1 t2 -> do
    ty1 <- typeof c t1
    typeof (ty1:c) t2

  Tuple ts -> do
    tys <- mapM (typeof c) ts
    pure (TyTuple tys)

  PrjT t n ->
    typeof c t >>= \case
      TyTuple ts -> pure (ts !! n)
      ty -> throwError ("Cannot project index "
                          <> tshow n
                          <> " from value of type "
                          <> tshow ty)

  Record ts0 -> do
    let (ls,ts) = unzip ts0
    tys <- mapM (typeof c) ts
    pure (TyRecord (zip ls tys))

  PrjR t l ->
    typeof c t >>= \case
      TyRecord ts0
        | Just ty <- lookup l ts0
            -> pure ty
      ty -> throwError ("Cannot project label "
                          <> l
                          <> " from value "
                          <> tshow ty)

  Inj l t (TyVariant tys) -> do
    ty  <- typeof c t
    case lookup l tys of
      Just ty' | ty == ty' ->
        pure (TyVariant tys)
      _ ->
        throwError ("Cannot inject label of type "
                      <> tshow ty
                      <> " into variant "
                      <> tshow (TyVariant tys))
  Inj _ _ ty -> throwError ("Cannot inject into type " <> tshow ty)

  -- TODO: Superfluous patterns should be a type error, e.g.
  --
  -- case x : {foo:Int, bar:Bool} of
  --   {foo,y} -> ...
  --   {bar,y} -> ...
  --   {baz,y} -> ... -- this should cause an error
  Case t bs ->
    typeof c t >>= \case
      TyVariant tys0 -> do
        tys <- forM tys0 (\(li,tyi) -> do
          case lookup2 li bs of
            Just (_,ti) -> typeof (tyi:c) ti
            Nothing ->
              throwError ("Missing pattern match on label "
                            <> li
                            <> " in branches "
                            <> tshow bs))
        case alleq tys of
          Nothing -> throwError ("Cannot have different types in case expression")
          Just ty -> pure ty
      ty -> throwError ("Cannot do case analysis on value of type " <> tshow ty)

  Fix t ->
    typeof c t >>= \case
      TyArr ty1 ty2 | ty1 == ty2 -> pure ty1
      ty -> throwError ("Cannot get fixpoint of value of type " <> tshow ty)

  Ref t -> do
    ty <- typeof c t
    pure (TyRef ty)

  Deref t ->
    typeof c t >>= \case
      TyRef ty -> pure ty
      ty -> throwError ("Cannot dereference value of type " <> tshow ty)

  Assign t1 t2 ->
    typeof c t1 >>= \case
      TyRef _ -> do
        _ <- typeof c t2 -- Typecheck RHS, but doesn't matter what it is
        pure TyUnit
      ty1 -> throwError ("Cannot assign to value of type " <> tshow ty1)

  Location l -> lookupType l

  Unit -> pure TyUnit

 where
  alleq :: Eq a => [a] -> Maybe a
  alleq []     = Nothing
  alleq (a:as) = go as
   where
    go [] = Just a
    go (b:bs)
      | a == b    = go bs
      | otherwise = Nothing

-- @fvs t@ finds all free variables in @t@.
fvs :: Term -> Set Int
fvs = go 0
 where
  go :: Int -> Term -> Set Int
  go !c = \case
    Var v
      | v >= c    -> Set.singleton v
      | otherwise -> mempty
    Lam _ _ t     -> go (c+1) t
    App t1 t2     -> go c t1 <> go c t2
    Asc t _       -> go c t
    Seq t1 t2     -> go c t1 <> go c t2
    Let _ t1 t2   -> go c t1 <> go (c+1) t2
    Tuple ts      -> foldMap (go c) ts
    PrjT t _      -> go c t
    Record ts     -> foldMap (go c . snd) ts
    PrjR t _      -> go c t
    Inj _ t _     -> go c t
    Case t bs     -> go c t <> foldMap (\(_,_,ti) -> go (c+1) ti) bs
    Fix t         -> go c t
    Ref t         -> go c t
    Deref t       -> go c t
    Assign t1 t2  -> go c t1 <> go c t2
    Location _    -> mempty
    Unit          -> mempty

-- @shift d t@ shifts all free variables in @t@ by @d@.
shift :: Int -> Term -> Term
shift d = go 0
 where
  go :: Int -> Term -> Term
  go !c = \case
    Var k
      | k < c     -> Var k
      | otherwise -> Var (k+d)
    Lam n ty t    -> Lam n ty (go (c+1) t)
    App t1 t2     -> App (go c t1) (go c t2)
    Asc t ty      -> Asc (go c t) ty
    Seq t1 t2     -> Seq (go c t1) (go c t2)
    Let n t1 t2   -> Let n (go c t1) (go (c+1) t2)
    Tuple ts      -> Tuple (map (go c) ts)
    PrjT t n      -> PrjT (go c t) n
    Record ts     -> Record (over (traverse._2) (go c) ts)
    PrjR t l      -> PrjR (go c t) l
    Inj l t ty    -> Inj l (go c t) ty
    Case t bs     -> Case (go c t) (over (traverse._3) (go (c+1)) bs)
    Fix t         -> Fix (go c t)
    Ref t         -> Ref (go c t)
    Deref t       -> Deref (go c t)
    Assign t1 t2  -> Assign (go c t1) (go c t2)
    Location l    -> Location l
    Unit          -> Unit

-- @subst x s t@ substitutes all free occurrences of @x@ in @t@ with @s@.
--
-- [x -> s]t
subst :: Subst -> Term -> Term
subst s0@(Subst x s) = \case
  Var v
    | x == v    -> s
    | otherwise -> Var v
  Lam n ty t    -> Lam n ty (under t)
  App t1 t2     -> App (subst s0 t1) (subst s0 t2)
  Asc t ty      -> Asc (subst s0 t) ty
  Seq t1 t2     -> Seq (subst s0 t1) (subst s0 t2)
  Let n t1 t2   -> Let n (subst s0 t1) (under t2)
  Tuple ts      -> Tuple (map (subst s0) ts)
  PrjT t n      -> PrjT (subst s0 t) n
  Record ts     -> Record (over (traverse._2) (subst s0) ts)
  PrjR t l      -> PrjR (subst s0 t) l
  Inj l t ty    -> Inj l (subst s0 t) ty
  Case t bs     -> Case (subst s0 t) (over (traverse._3) under bs)
  Fix t         -> Fix (subst s0 t)
  Ref t         -> Ref (subst s0 t)
  Deref t       -> Deref (subst s0 t)
  Assign t1 t2  -> Assign (subst s0 t1) (subst s0 t2)
  Location l    -> Location l
  Unit          -> Unit
 where
  -- Substitute under a lambda: shift the inner term and apply the substitution
  -- on the shifted index instead.
  under :: Term -> Term
  under = subst (Subst (x+1) (shift 1 s))

-- Small-step call-by-value evaluation
eval :: Term -> StateT Store Maybe Term
eval = \case
  Var _ -> lift Nothing

  Lam _ _ _ -> lift Nothing

  App (Lam _ _ t) v | isval v ->
    pure (beta v t)
  App v@(Lam _ _ _) t2 -> do
    t2' <- eval t2
    pure (App v t2')
  App t1 t2 -> do
    t1' <- eval t1
    pure (App t1' t2)

  Asc v _ | isval v ->
    pure v
  Asc t ty -> do
    t' <- eval t
    pure (Asc t' ty)

  Seq Unit t2 ->
    pure t2
  Seq t1 t2 -> do
    t1' <- eval t1
    pure (Seq t1' t2)

  Let _ v t | isval v ->
    pure (beta v t)
  Let n t1 t2 -> do
    t1' <- eval t1
    pure (Let n t1' t2)

  Tuple ts0 | (vs,t:ts) <- span isval ts0 -> do
    t' <- eval t
    pure (Tuple (vs ++ [t'] ++ ts))
  Tuple _ -> lift Nothing

  PrjT (Tuple ts) n | all isval ts ->
    pure (ts !! n)
  PrjT t n -> do
    t' <- eval t
    pure (PrjT t' n)

  PrjR (Record ts) l | all (isval . snd) ts ->
    lift (lookup l ts)
  PrjR t l -> do
    t' <- eval t
    pure (PrjR t' l)

  Record ts0 -> do
    let (ls,ts) = unzip ts0
    case span isval ts of
      (vs,t:tss) -> do
        t' <- eval t
        pure (Record (zip ls (vs ++ [t'] ++ tss)))
      _ -> lift Nothing

  -- FIXME: Do I need a "| not (isval t)" guard?
  Inj l t ty -> do
    t' <- eval t
    pure (Inj l t' ty)

  Case (Inj l v _) bs | isval v -> do
    (_,t) <- lift (lookup2 l bs)
    pure (beta v t)
  Case t bs -> do
    t' <- eval t
    pure (Case t' bs)

  t0@(Fix (Lam _ _ t)) ->
    pure (subst (Subst 0 t0) t)
  Fix t -> do
    t' <- eval t
    pure (Fix t')

  Ref t | isval t -> do
    l <- storeNewValue t
    pure (Location l)
  Ref t -> do
    t' <- eval t
    pure (Ref t')

  Deref (Location l) ->
    lookupValue l
  Deref t -> do
    t' <- eval t
    pure (Deref t')

  Assign (Location l) t | isval t -> do
    storeValue l t
    pure Unit
  Assign t1 t2 | isval t1 -> do
    t2' <- eval t2
    pure (Assign t1 t2')
  Assign t1 t2 -> do
    t1' <- eval t1
    pure (Assign t1' t2)

  Location _ -> lift Nothing

  Unit -> lift Nothing

 where
  -- @beta s t@ subtitutes s for variable 0 in t.
  beta :: Term -> Term -> Term
  beta s t = shift (-1) (subst (Subst 0 (shift 1 s)) t)


isval :: Term -> Bool
isval = \case
  Lam _ _ _  -> True
  Tuple ts   -> all isval ts
  Record ts  -> all (isval . snd) ts
  Inj _ t _  -> isval t
  Location _ -> True
  Unit       -> True
  _          -> False

-- Big-step call-by-value evaluation
-- eval' :: Term -> Term
-- eval' t0 = maybe t0 eval' (eval t0)

--------------------------------------------------------------------------------

lookup2 :: Eq a => a -> [(a,b,c)] -> Maybe (b,c)
lookup2 _ [] = Nothing
lookup2 a ((a',b,d):xs)
  | a == a'   = Just (b,d)
  | otherwise = lookup2 a xs

assertM :: Monad m => Bool -> m ()
assertM b = assert b `seq` pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show
