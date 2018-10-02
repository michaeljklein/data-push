{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Push2 where

import Control.Comonad
import Control.Comonad.Cofree
import Data.Bifunctor (Bifunctor(..))
import Data.Either (isLeft, isRight)
import Data.Function (on)
import Data.Functor.Const (Const)
import Data.List (groupBy)

-- import Data.Functor.Adjunction (Adjunction(..), cozipL, duplicateL)

-- | `Right` when the predicate returns `True`
pushP2 ::
     Push2 f Either => (a -> Bool) -> f a -> Pushed2 f Either (Either (f a) (f a))
pushP2 p =
  pmap2 $ \x ->
    if p x
      then Right x
      else Left x

-- | `fmap` and `push2`
pmap2 :: Push2 f g => (a -> g b c) -> f a -> Pushed2 f g (g (f b) (f c))
pmap2 f = push2 . fmap f


-- | Push through a binary functor (`Bifunctor`)
class (Functor f, Bifunctor g) =>
      Push2 f g where
  type Pushed2 f g a :: *

  pushed2 :: Pushed2 f g a -> Const (f a) (g a)
  part2 :: f (g a b) -> f (f (g a b))
  pver2 :: f (f (g a b)) -> Pushed2 f g (g (f a) (f b))

  -- | push2 = pver2 . part2
  push2 :: f (g a b) -> Pushed2 f g (g (f a) (f b))
  push2 = pver2 . part2

  -- | A witness to the fact that the innermost @f@'s in @Pushed2 f (g (f a) (f b))@ is inhabited
  push21 :: f (g a b) -> Pushed2 f g (g a b) -- instance Push2 [] Either where

--   part2 :: [Either a b] -> [[Either a b]]
--   part2 = groupBy ((==) `on` isLeft)
--   pver2 :: [[Either a b]] -> [Either [a] [b]]
--   pver2 = fmap pver2'
--     where
--       pver2' [] = error "pver2': []"
--       pver2' xs@(Left _:_) = Left ((\(~(Left x)) -> x) <$> xs)
--       pver2' xs = Right ((\(~(Right x)) -> x) <$> xs)

-- instance Push2 (Cofree Maybe) Either where
--   pver2 ::
--        Cofree Maybe (Cofree Maybe (Either a b))
--     -> Cofree Maybe (Either (Cofree Maybe a) (Cofree Maybe b))
--   pver2 = fmap pver2'
--     where
--       pver2' xs@(Left _ :< _) = Left $ (\(~(Left x)) -> x) <$> xs
--       pver2' xs = Right $ (\(~(Right x)) -> x) <$> xs
--
--   part2 :: Cofree Maybe (Either a b) -> Cofree Maybe (Cofree Maybe (Either a b))
--   part2 xss@(~(x :< xs)) =
--     case xs of
--       Nothing -> xss :< Nothing
--       ~(Just xs') ->
--         case x of
--           Left _ ->
--             case part2 xs' of
--               (ys@(Left _ :< _) :< zs) -> (x :< Just ys) :< zs
--               ys -> (x :< Nothing) :< Just ys
--           ~(Right _) ->
--             case part2 xs' of
--               (ys@(Right _ :< _) :< zs) -> (x :< Just ys) :< zs
--               ys -> (x :< Nothing) :< Just ys


