{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}

module Utils where

import Data.Functor.Identity


type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s


over :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens get set f s = set s <$> f (get s)


class Field1 s a | s -> a where
  _1 :: Lens s a

instance Field1 (a,b) a where
  _1 = lens fst (\(_,b) a -> (a,b))


class Field2 s a | s -> a where
  _2 :: Lens s a

instance Field2 (a,b) b where
  _2 = lens snd (\(a,_) b -> (a,b))


class Field3 s a | s -> a where
  _3 :: Lens s a

instance Field3 (a,b,c) c where
  _3 = lens (\(_,_,c) -> c) (\(a,b,_) c -> (a,b,c))
