{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.End where

import Data.End (HasEnd(..))
import GHC.Generics
import Data.List.End (EndList(..))
import Data.Tree (Tree(..))
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad (Comonad(..))
import Control.Comonad.Env.Class (ComonadEnv(..))

import Data.Biapplicative (Biapplicative(..), biliftA2)
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))


data EndTree e a = EndNode { endRootLabel :: a
                           , subEndForest :: EndForest e a
                           } deriving (Eq, Ord, Show, Functor, Generic, Generic1)

newtype EndForest e a = EndForest { getEndForet :: EndList e (EndTree e a) } deriving (Eq, Ord, Show, Functor, Generic, Generic1)


instance HasEnd EndTree where
  end = end . subEndForest

instance Bifunctor EndTree where
  bimap f g ~(EndNode x xs) = EndNode (g x) $ bimap f g xs

instance Biapplicative EndTree where
  bipure x y = EndNode y (EndForest (End x))

  ~(EndNode f fs) <<*>> ~(EndNode x xs) = EndNode (f x) $ fs <<*>> xs

instance Bifoldable EndTree where
  bifoldr f g x ~(EndNode y ys) = bifoldr f g (g y x) ys

instance Bitraversable EndTree where
  bitraverse f g ~(EndNode x xs) = EndNode <$> g x <*> bitraverse f g xs

instance Foldable (EndTree e) where
  foldr f x ~(EndNode y ys) = foldr f (f y x) ys

instance Traversable (EndTree e) where
  traverse f ~(EndNode x xs) = EndNode <$> f x <*> traverse f xs

instance Comonad (EndTree e) where
  extract = endRootLabel

  duplicate xss@(~(EndNode _ (EndForest xs))) = EndNode xss . EndForest . fmap duplicate $ xs

instance ComonadEnv e (EndTree e) where
  ask = end



instance HasEnd EndForest where
  end (EndForest (End x)) = x
  end ~(EndForest (x :. _)) = end x

instance Bifunctor EndForest where
  bimap f g ~(EndForest xs) = EndForest $ bimap f (bimap f g) xs

instance Biapplicative EndForest where
  bipure x y = EndForest $ bipure x y :. End x

  ~(EndForest fs) <<*>> ~(EndForest xs) = EndForest $ biliftA2 ($) (<<*>>) fs xs

instance Bifoldable EndForest where
  bifoldr f g x ~(EndForest xs) = bifoldr f (flip $ bifoldr f g) x xs

instance Bitraversable EndForest where
  bitraverse f g ~(EndForest xs) = EndForest <$> bitraverse f (bitraverse f g) xs

instance Foldable (EndForest e) where
  foldr f x ~(EndForest xs) = foldr (flip $ foldr f) x xs

instance Traversable (EndForest e) where
  traverse f ~(EndForest xs) = EndForest <$> traverse (traverse f) xs


-- Cofree conversion utilities

cofreeListToTree :: Cofree [] a -> Tree a
cofreeListToTree ~(x :< xs) = Node x $ fmap cofreeListToTree xs

treeToCofreeList :: Tree a -> Cofree [] a
treeToCofreeList ~(Node x xs) = x :< fmap treeToCofreeList xs


