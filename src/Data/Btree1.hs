{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Btree1 where

import Control.Comonad
import GHC.Generics
import Data.Btree (Btree(..))

data Btree1 a = Bnode1 { bnode1  :: a
                       , bnodel1 :: Btree a
                       , bnoder1 :: Btree a
                       } deriving (Eq, Ord, Show, Functor, Generic, Generic1)

instance Applicative Btree1 where
  pure x = Bnode1 x (pure x) (pure x)

  ~(Bnode1 fx fxs fys) <*> ~(Bnode1 fz fzs fws) = Bnode1 (fx fz) (fxs <*> fzs) (fys <*> fws)

instance Comonad Btree1 where
  extract = bnode1

  duplicate xss@(~(Bnode1 _ ys zs)) = Bnode1 xss (maybe Bnil (toBtree . duplicate) $ fromBtree ys) (maybe Bnil (toBtree . duplicate) $ fromBtree zs)

instance Foldable Btree1 where
  foldr f x ~(Bnode1 y ys _) = foldr f (f y x) ys

instance Traversable Btree1 where
  traverse f ~(Bnode1 x xs ys) = Bnode1 <$> f x <*> traverse f xs <*> traverse f ys

-- | Helpers

fromBtree :: Btree a -> Maybe (Btree1 a)
fromBtree Bnil = Nothing
fromBtree ~(Bnode x xs ys) = Just $ Bnode1 x xs ys

toBtree :: Btree1 a -> Btree a
toBtree ~(Bnode1 x xs ys) = Bnode x xs ys

