{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.List.End where

import Control.Applicative
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Free (Free(..))
import Control.Monad.Trans.Free (FreeT(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Biapplicative (Biapplicative(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics
import qualified Control.Monad.Trans.Free as F

import Data.End (HasEnd(..))

data EndList e a = End e | (:.) a (EndList e a) deriving (Eq, Ord, Functor, Generic, Generic1)

instance HasEnd EndList where
  end (End x) = x
  end ~(_ :. xs) = end xs

instance (Show e, Show a) => Show (EndList e a) where
  show (End x) = "End " ++ show x
  show (x :. xs) = show x ++ " :. " ++ show xs

instance Bifunctor EndList where
  bimap f _ (End x) = End (f x)
  bimap f g ~(x :. xs) = g x :. bimap f g xs

instance Biapplicative EndList where
  bipure x y = y :. End x

  End f <<*>> xs = End . f . end $ xs
  (_ :. fs) <<*>> End x = fs <<*>> End x
  ~(f :. fs) <<*>> ~(x :. xs) = f x :. (fs <<*>> xs)

instance Bifoldable EndList where
  bifoldr f _ x (End y) = f y x
  bifoldr f g x ~(y :. ys) = bifoldr f g (g y x) ys

instance Bitraversable EndList where
  bitraverse f _ (End x) = End <$> f x
  bitraverse f g ~(x :. xs) = (:.) <$> g x <*> bitraverse f g xs

instance Foldable (EndList e) where
  foldr _ x (End _) = x
  foldr f x ~(y :. ys) = foldr f (f y x) ys

instance Traversable (EndList e) where
  traverse _ (End x) = pure $ End x
  traverse f ~(x :. xs) = (:.) <$> f x <*> traverse f xs

instance Monoid e => Applicative (EndList e) where
  pure = (:. empty)

  End x <*> _ = End x
  _ <*> End x = End x
  (f :. fs) <*> (x :. xs) = f x :. (fs <*> xs)

instance Monoid e => Alternative (EndList e) where
  empty = End mempty

  End x <|> y = mappend x `first` y
  x <|> End y = mappend y `first` x
  ~(x :. xs) <|> ys = x :. (xs <|> ys)

instance Monoid e => Monad (EndList e) where
  fail _ = End mempty

  End x >>= _ = End x
  (x :. xs) >>= f = f x <|> (xs >>= f)

instance Monoid e => MonadZip (EndList e) where
  mzip (End x) (End y) = End $ mappend x y
  mzip (End x) _ = End x
  mzip _ (End y) = End y
  mzip ~(x :. xs) ~(y :. ys) = (x, y) :. mzip xs ys

  mzipWith _ (End x) (End y) = End $ mappend x y
  mzipWith _ (End x) _ = End x
  mzipWith _ _ (End y) = End y
  mzipWith f ~(x :. xs) ~(y :. ys) = f x y :. mzipWith f xs ys

instance Monoid e => MonadFix (EndList e) where
  mfix f = case fix (f . ehead) of
             End x -> End x
             ~(x :. _) -> x :. mfix (etail . f)



-- EndList utilities

fromNonEmptyList :: NonEmpty a -> EndList a a
fromNonEmptyList ~(x :| xs) = case xs of
                                [] -> End x
                                ~(y:ys) -> x :. fromNonEmptyList (y :| ys)

fromListWithEnd :: e -> [a] -> EndList e a
fromListWithEnd x [] = End x
fromListWithEnd x ~(y:ys) = y :. fromListWithEnd x ys

fromListWithEndF :: (a -> e -> e) -> e -> [a] -> EndList e a
fromListWithEndF _ x [] = End x
fromListWithEndF f x ~(y:ys) = y :. fromListWithEndF f (f y x) ys

fromListWithEndA :: Applicative f => (a -> f e -> f e) -> f e -> [a] -> f (EndList e a)
fromListWithEndA _ x [] = End <$> x
fromListWithEndA f x ~(y:ys) = (:.) <$> pure y <*> fromListWithEndA f (f y x) ys

ehead :: EndList e a -> a
{-# INLINE ehead #-}
ehead ~(x :. _) = x

etail :: EndList e a -> EndList e a
{-# INLINE etail #-}
etail (End x) = End x
etail ~(_ :. xs) = xs

econcat :: Monoid e => EndList e (EndList e a) -> EndList e a
econcat = econcatWith mappend

econcatWith :: (e -> e -> e) -> EndList e (EndList e a) -> EndList e a
econcatWith _ (End x) = End x
econcatWith f ~(x :. xs) = case x of
                             End y -> f y `first` econcatWith f xs
                             ~(y :. ys) -> y :. econcatWith f (ys :. xs)

endListToFree :: EndList e a -> Free ((,) a) e
endListToFree (End x) = Pure x
endListToFree ~(x :. xs) = Free (x, endListToFree xs)

freeToEndList :: Free ((,) a) e -> EndList e a
freeToEndList (Pure x) = End x
freeToEndList ~(Free (x, xs)) = x :. freeToEndList xs


endListToFreeM :: Monad m => EndList e a -> FreeT ((,) a) m e
endListToFreeM (End x) = FreeT . return . F.Pure $ x
endListToFreeM ~(x :. xs) = FreeT . return . F.Free $ (x, endListToFreeM xs)

freeToEndListM :: Monad m => FreeT ((,) a) m e -> m (EndList e a)
freeToEndListM = (>>= go) . runFreeT
  where
    go (F.Pure x) = return . End $ x
    go ~(F.Free (x, xs)) = (x :.) <$> freeToEndListM xs


