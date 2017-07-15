{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module UntypedExpr where

-- t  ::=                    -- terms:
--        true               -- constant true
--        false              -- constant false
--        if t then t else t -- conditional
--        0                  -- constant zero
--        succ t             -- successor
--        pred t             -- predecessor
--        iszero t           -- zero test

-- v  ::=                    -- values:
--        true               -- true value
--        false              -- false value
--        nv                 -- numeric value

-- nv ::=                   -- numeric values:
--        0                 -- zero value
--        succ nv           -- successor value

data U
  = Tru
  | Fls
  | Ifte U U U
  | Zero
  | Succ U
  | Pred U
  | IsZero U
  deriving (Eq, Show)

-- Small-step evaluation
eval :: U -> Maybe U
eval = \case
  Ifte u1 u2 u3 ->
    case u1 of
      -- E-IfTrue
      Tru -> pure u2
      -- E-IfFalse
      Fls -> pure u3
      -- E-If
      _ -> do
        !u1' <- eval u1
        pure (Ifte u1' u2 u3)

  -- E-Succ
  Succ u -> do
    !u' <- eval u
    pure (Succ u')

  Pred u1 ->
    case u1 of
      -- E-PredZero
      Zero -> pure Zero
      -- E-PredSucc
      Succ Zero      -> pure Zero
      Succ (Succ u2) -> pure (Succ u2)
      -- E-Pred
      _ -> do
        !u1' <- eval u1
        pure (Pred u1')

  IsZero u ->
    case u of
      -- E-IsZeroZero
      Zero -> pure Tru
      -- E-IsZeroSucc
      Succ Zero     -> pure Fls
      Succ (Succ _) -> pure Fls
      -- E-IsZero
      _ -> do
        !u' <- eval u
        pure (IsZero u')

  _ -> Nothing
