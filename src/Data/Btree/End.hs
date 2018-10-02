{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Btree.End where

import Data.End (HasEnd(..))
import GHC.Generics
import Data.Biapplicative (Biapplicative(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))


data EndBtree e a = Ebnil e
                  | Ebnode { ebnode  :: a
                           , ebnodel :: EndBtree e a
                           , ebnoder :: EndBtree e a
                           }
                  deriving (Eq, Ord, Show, Functor, Generic, Generic1)

instance HasEnd EndBtree where
  end (Ebnil x) = x
  end ~(Ebnode _ xs _) = end xs

instance Bifunctor EndBtree where
  bimap f _ (Ebnil x) = Ebnil $ f x
  bimap f g ~(Ebnode x xs ys) = Ebnode (g x) (bimap f g xs) (bimap f g ys)

instance Biapplicative EndBtree where
  bipure x y = Ebnode y z z
    where
      z = Ebnil x

  Ebnil f <<*>> xs = Ebnil . f . end $ xs
  ~(Ebnode f fs gs) <<*>> ~(Ebnode x xs ys) = Ebnode (f x) (fs <<*>> xs) (gs <<*>> ys)

instance Bifoldable EndBtree where
  bifoldr f _ x (Ebnil y) = f y x
  bifoldr f g x ~(Ebnode y ys zs) = bifoldr f g (bifoldr f g (g y x) zs) ys

instance Bitraversable EndBtree where
  bitraverse f _ (Ebnil x) = Ebnil <$> f x
  bitraverse f g ~(Ebnode x xs ys) = Ebnode <$> g x <*> bitraverse f g xs <*> bitraverse f g ys

instance Foldable (EndBtree e) where
  foldr _ x (Ebnil _) = x
  foldr f x ~(Ebnode y ys zs) = foldr f (foldr f (f y x) zs) ys

instance Traversable (EndBtree e) where
  traverse _ (Ebnil x) = pure $ Ebnil x
  traverse f ~(Ebnode x xs ys) = Ebnode <$> f x <*> traverse f xs <*> traverse f ys



