{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Btree1.End where

import Data.Btree.End (EndBtree(..))
import Data.End (HasEnd(..))
import GHC.Generics
import Data.Biapplicative (Biapplicative(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))

import Control.Comonad (Comonad(..))
import Control.Comonad.Env.Class (ComonadEnv(..))


data EndBtree1 e a = Ebnode1 { ebnode1  :: a
                             , ebnodel1 :: EndBtree e a
                             , ebnoder1 :: EndBtree e a
                             } deriving (Eq, Ord, Functor, Generic, Generic1)

instance HasEnd EndBtree1 where
  end ~(Ebnode1 _ xs _) = end xs

instance Bifunctor EndBtree1 where
  bimap f g ~(Ebnode1 x xs ys) = Ebnode1 (g x) (bimap f g xs) (bimap f g ys)

instance Biapplicative EndBtree1 where
  bipure x y = Ebnode1 y z z
    where
      z = Ebnil x

  ~(Ebnode1 f fs gs) <<*>> ~(Ebnode1 x xs ys) = Ebnode1 (f x) (fs <<*>> xs) (gs <<*>> ys)

instance Bifoldable EndBtree1 where
  bifoldr f g x ~(Ebnode1 y ys zs) = bifoldr f g (bifoldr f g (g y x) zs) ys

instance Bitraversable EndBtree1 where
  bitraverse f g ~(Ebnode1 x xs ys) = Ebnode1 <$> g x <*> bitraverse f g xs <*> bitraverse f g ys

instance Foldable (EndBtree1 e) where
  foldr f x ~(Ebnode1 y ys zs) = foldr f (foldr f (f y x) zs) ys

instance Traversable (EndBtree1 e) where
  traverse f ~(Ebnode1 x xs ys) = Ebnode1 <$> f x <*> traverse f xs <*> traverse f ys

instance Comonad (EndBtree1 e) where
  extract = ebnode1

  duplicate xss@(~(Ebnode1 _ xs ys)) = Ebnode1 xss (duplicateSub xs) (duplicateSub ys)
    where
      duplicateSub = either Ebnil (fromEndBtree1 . duplicate) . toEndBtree1

instance ComonadEnv e (EndBtree1 e) where
  ask = end


-- Helpers

toEndBtree1 :: EndBtree e a -> Either e (EndBtree1 e a)
toEndBtree1 (Ebnil x) = Left x
toEndBtree1 ~(Ebnode x xs ys) = Right $ Ebnode1 x xs ys

fromEndBtree1 :: EndBtree1 e a -> EndBtree e a
fromEndBtree1 ~(Ebnode1 x xs ys) = Ebnode x xs ys

