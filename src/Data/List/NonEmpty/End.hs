{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.List.NonEmpty.End where

import Data.End (HasEnd(..))
import GHC.Generics
import Data.Biapplicative (Biapplicative(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Control.Comonad (Comonad(..))
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad.Env.Class (ComonadEnv(..))
import Data.List.End (EndList(..), fromListWithEnd, fromListWithEndA)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (toList)

data NonEmptyEnd e a = (:|.) a (EndList e a) deriving (Eq, Ord, Show, Functor, Generic, Generic1)

instance HasEnd NonEmptyEnd where
  end ~(_ :|. xs) = end xs

instance Bifunctor NonEmptyEnd where
  bimap f g ~(x :|. xs) = g x :|. bimap f g xs

instance Biapplicative NonEmptyEnd where
  bipure x y = y :|. End x

  ~(f :|. fs) <<*>> ~(x :|. xs) = f x :|. (fs <<*>> xs)

instance Bifoldable NonEmptyEnd where
  bifoldr f g x ~(y :|. ys) = bifoldr f g (g y x) ys

instance Bitraversable NonEmptyEnd where
  bitraverse f g ~(x :|. xs) = (:|.) <$> g x <*> bitraverse f g xs

instance Foldable (NonEmptyEnd e) where
  foldr f x ~(y :|. ys) = foldr f (f y x) ys

instance Traversable (NonEmptyEnd e) where
  traverse f ~(x :|. xs) = (:|.) <$> f x <*> traverse f xs

instance Comonad (NonEmptyEnd e) where
  extract ~(x :|. _) = x

  extend f xss@(~(_ :|. xs)) = f xss :|. case xs of
                                           End y -> End y
                                           ~(y :. ys) -> toEndList1 $ extend f (y :|. ys)

instance ComonadEnv e (NonEmptyEnd e) where
  ask = end


-- NonEmptyEnd utilities

fromListWithEnd1 :: e -> NonEmpty a -> NonEmptyEnd e a
fromListWithEnd1 x ~(y :| ys) = y :|. fromListWithEnd x ys

fromListWithEnd1F :: (a -> e -> e) -> e -> NonEmpty a -> NonEmptyEnd e a
fromListWithEnd1F f x ~(y :| ys) = y :|. case ys of
                                           [] -> End x
                                           ~(z:zs) -> toEndList1 $ fromListWithEnd1F f x (z :| zs)

fromListWithEnd1A :: Applicative f => (a -> f e -> f e) -> f e -> NonEmpty a -> f (NonEmptyEnd e a)
fromListWithEnd1A f x ~(y :| ys) = (y :|.) <$> fromListWithEndA f (f y x) ys

toEndList1 :: NonEmptyEnd e a -> EndList e a
toEndList1 ~(x :|. xs) = x :. xs

fromEndList1 :: EndList e a -> Either e (NonEmptyEnd e a)
fromEndList1 (End x) = Left x
fromEndList1 ~(x :. xs) = Right $ x :|. xs


-- Misc. utilities

cofreeMaybeToNonEmpty :: Cofree Maybe a -> NonEmpty a
cofreeMaybeToNonEmpty ~(x :< xs) = x :| case xs of
                                          Nothing -> []
                                          ~(Just ys) -> toList $ cofreeMaybeToNonEmpty ys

nonEmptyToCofreeMaybe :: NonEmpty a -> Cofree Maybe a
nonEmptyToCofreeMaybe ~(x :| xs) = x :< case xs of
                                          [] -> Nothing
                                          ~(y:ys) -> Just $ nonEmptyToCofreeMaybe (y :| ys)

cofreeMaybeToNonEmptyM :: Monad m => Cofree (MaybeT m) a -> m (NonEmpty a)
cofreeMaybeToNonEmptyM ~(x :< MaybeT xs) = fmap (x :|) $ do
  xs' <- xs
  case xs' of
    Nothing -> return []
    ~(Just ys) -> toList <$> cofreeMaybeToNonEmptyM ys

nonEmptyToCofreeMaybeM :: Monad m => NonEmpty a -> Cofree (MaybeT m) a
nonEmptyToCofreeMaybeM ~(x :| xs) = (x :<) . MaybeT . return $ case xs of
                                                                 [] -> Nothing
                                                                 ~(y:ys) -> Just $ nonEmptyToCofreeMaybeM (y :| ys)

cofreeEitherToNonEmptyEnd :: Cofree (Either e) a -> NonEmptyEnd e a
cofreeEitherToNonEmptyEnd ~(x :< xs) = x :|. case xs of
                                               Left y -> End y
                                               ~(Right ys) -> toEndList1 $ cofreeEitherToNonEmptyEnd ys

nonEmptyEndToCofreeEither :: NonEmptyEnd e a -> Cofree (Either e) a
nonEmptyEndToCofreeEither ~(x :|. xs) = x :< case xs of
                                               End y -> Left y
                                               ~(y :. ys) -> Right $ nonEmptyEndToCofreeEither (y :|. ys)

cofreeExceptToNonEmptyEnd :: Monad m => Cofree (ExceptT e m) a -> m (NonEmptyEnd e a)
cofreeExceptToNonEmptyEnd ~(x :< ExceptT xs) = fmap (x :|.) $ do
  xs' <- xs
  case xs' of
    Left y -> return $ End y
    ~(Right ys) -> toEndList1 <$> cofreeExceptToNonEmptyEnd ys

nonEmptyEndToCofreeExcept :: Monad m => NonEmptyEnd e a -> Cofree (ExceptT e m) a
nonEmptyEndToCofreeExcept ~(x :|. xs) = (x :<) . ExceptT . return $ case xs of
                                                                      End y -> Left y
                                                                      ~(y :. ys) -> Right $ nonEmptyEndToCofreeExcept (y :|. ys)

