{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Btree where

import GHC.Generics
import Data.Tree (Tree(..), Forest)

data Btree a = Bnil
             | Bnode { bnode  :: a
                     , bnodel :: Btree a
                     , bnoder :: Btree a
                     }
             deriving (Eq, Ord, Show, Functor, Generic, Generic1)

instance Foldable Btree where
  foldr _ x Bnil = x
  foldr f x ~(Bnode y ys zs) = foldr f (foldr f (f y x) ys) zs

instance Traversable Btree where
  traverse _ Bnil = pure Bnil
  traverse f ~(Bnode x xs ys) = Bnode <$> f x <*> traverse f xs <*> traverse f ys

instance Applicative Btree where
  pure x = Bnode x Bnil Bnil

  Bnil <*> _ = Bnil
  _ <*> Bnil = Bnil
  ~(Bnode f gs hs) <*> ~(Bnode x xs ys) = Bnode (f x) (gs <*> xs) (hs <*> ys)


-- | Helpers

toTree :: Btree a -> Maybe (Tree a)
toTree Bnil = Nothing
toTree ~(Bnode x xs ys) = Just . Node x $ [xs, ys] >>= toForest

toForest :: Btree a -> Forest a
toForest Bnil = []
toForest ~(Bnode x xs ys) = [Node x $ [xs, ys] >>= toForest]


-- | Generate a `Btree` of constant depth:
--
-- @
-- constDepth 1 = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
-- constDepth 2 =
--   Bnode
--     { bnode = 2
--     , bnodel = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
--     , bnoder = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
--     }
-- constDepth 3 =
--   Bnode
--     { bnode = 3
--     , bnodel =
--         Bnode
--           { bnode = 2
--           , bnodel = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
--           , bnoder = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
--           }
--     , bnoder =
--         Bnode
--           { bnode = 2
--           , bnodel = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
--           , bnoder = Bnode {bnode = 1, bnodel = Bnil, bnoder = Bnil}
--           }
--     }
-- @
--
-- A quick search finds the sizes on OEIS: http://oeis.org/A000225
-- @
-- λ> all (\n -> 2^n-1 == (length . constDepth) n) [1..20 :: Int]
-- True
-- (1.53 secs, 1,342,668,640 bytes)constDepth :: (Enum a, Ord a) => a -> Btree a
-- @
--
constDepth x
  | x <= toEnum 0 = Bnil
  | otherwise = Bnode x ys ys
  where
    ys = constDepth (pred x)

