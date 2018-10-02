{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Push where

-- import Control.Applicative
-- import Control.Comonad (Comonad(..))
-- import Control.Comonad.Cofree (Cofree(..))
-- import Control.JoinR
-- import Control.Monad.Free (Free(..))
-- import Control.Monad.ST
-- import Control.Monad.Trans.Free (FreeT(..))
-- import Control.Qush
-- import Data.Bifunctor
-- import Data.Either (isLeft)
-- import Data.Foldable (fold, toList)
-- import Data.Function (on)
-- import Data.Functor.Compose (Compose(..))
-- import Data.Functor.Const
-- import Data.Functor.Contravariant (Contravariant(..))
-- import Data.List (groupBy)
-- import Data.Maybe (fromJust, isJust)
-- import Data.Tree (Tree(..))

import Control.Applicative
import Control.Monad (join)
import Data.Function
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..), groupBy1)
import GHC.Generics


-- | Warning: Incomplete instance
instance (Applicative f, Applicative g) => Applicative (f :+: g) where
  pure = R1 . pure

  L1 f <*> L1 x = L1 (f <*> x)
  R1 f <*> R1 x = R1 (f <*> x)


class (Functor f, Functor g) => Part f g where
  part :: f (g a) -> f (f (g a))

instance (Push f g, Part g h) => Part (f :.: g) h where
  part :: (f :.: g) (h a) -> (f :.: g) ((f :.: g) (h a))
  part = Comp1 . fmap (fmap Comp1) . push . fmap part . unComp1

instance (Part f g, Functor h) => Part f (g :.: h) where
  part :: f ((g :.: h) a) -> f (f ((g :.: h) a))
  part = fmap (fmap Comp1) . part . fmap unComp1

instance (Part f h, Part g h) => Part (f :+: g) h where
  part :: (f :+: g) (h a) -> (f :+: g) ((f :+: g) (h a))
  part (L1 x) = L1 . fmap L1 . part $ x
  part ~(R1 x) = R1 . fmap R1 . part $ x

instance (Part f g, Part f h, Applicative f) => Part f (g :*: h) where
  part :: f ((g :*: h) a) -> f (f ((g :*: h) a))
  part x = liftA2 (liftA2 (:*:)) (part ((\(y :*: _) -> y) <$> x)) (part ((\(_ :*: y) -> y) <$> x))

instance (Part f h, Part g h) => Part (f :*: g) h where
  part :: (f :*: g) (h a) -> (f :*: g) ((f :*: g) (h a))
  part ((:*:) x y) = fmap (:*: y) (part x) :*: fmap (x :*:) (part y)

instance (Part [] g, Part [] h) => Part [] (g :+: h) where
  part = withNonEmpty $ concatMap unSum1 . groupBy1 ((==) `on` isL1)
    where
      withNonEmpty :: (NonEmpty a -> [b]) -> [a] -> [b]
      withNonEmpty _ [] = []
      withNonEmpty f ~(x:xs) = f (x :| xs)

      isL1 :: (f :+: g) a -> Bool
      isL1 (L1 _) = True
      isL1 _ = False

      unSum1 :: (Part [] f, Part [] g) => NonEmpty ((f :+: g) a) -> [[(f :+: g) a]]
      unSum1 ~(x :| xs) = case x of
                            ( L1 x') -> fmap (fmap L1) . part $ x' : fmap (\(~(L1 y)) -> y) xs
                            ~(R1 x') -> fmap (fmap R1) . part $ x' : fmap (\(~(R1 y)) -> y) xs

instance {-# OVERLAPPABLE #-} Part f g => Part (M1 i c f) g where
  part = M1 . fmap M1 . part . unM1

instance {-# OVERLAPPABLE #-} Part f g => Part f (M1 i c g) where
  part = fmap (fmap M1) . part . fmap unM1

instance {-# OVERLAPPABLE #-} Part f g => Part (M1 i c f) (M1 j d g) where
  part = M1 . fmap (M1 . fmap M1) . part . fmap unM1 . unM1


instance Functor f => Part (K1 i c) f where
  part = K1 . unK1

instance Monad f => Part f (K1 i c) where
  part = return

instance Functor f => Part (Const c) f where
  part = Const . getConst

instance Monad f => Part f (Const c) where
  part = fmap return

instance Functor f => Part U1 f where
  part = const U1

instance Monad f => Part f U1 where
  part = fmap return

instance Functor f => Part Identity f where
  part = return

instance Monad f => Part f Identity where
  part = return

instance Functor f => Part Par1 f where
  part = return

instance Monad f => Part f Par1 where
  part = return

instance Part f g => Part (Rec1 f) g where
  part = Rec1 . fmap Rec1 . part . unRec1

instance Part f g => Part f (Rec1 g) where
  part = fmap (fmap Rec1) . part . fmap unRec1


class (Functor f, Functor g) => Dart f g where
  dart :: f (f (g a)) -> f (g (f a))

instance (Push g h, Push f h, Pull f g, Pull g f) => Dart (f :.: g) h where
  dart :: (f :.: g) ((f :.: g) (h a)) -> (f :.: g) (h ((f :.: g) a))
  dart =
    fmap (fmap Comp1) .
    Comp1 .
    pull . fmap (fmap push . pull . fmap (fmap push . unComp1)) . unComp1

instance (Dart f g, Pull f g, Push f h) => Dart f (g :.: h) where
  dart :: f (f ((g :.: h) a)) -> f ((g :.: h) (f a))
  dart = fmap Comp1 . pull . fmap (fmap push) . dart . fmap (fmap unComp1)

instance (Dart f h, Dart g h) => Dart (f :+: g) h where
  dart :: (f :+: g) ((f :+: g) (h a)) -> (f :+: g) (h ((f :+: g) a))
  dart (L1 x) = L1 . fmap (fmap L1) . dart . fmap (\(~(L1 y)) -> y) $ x
  dart ~(R1 x) = R1 . fmap (fmap R1) . dart . fmap (\(~(R1 y)) -> y) $ x

instance (Dart f g, Dart f h, Applicative f) => Dart f (g :*: h) where
  dart :: f (f ((g :*: h) a)) -> f ((g :*: h) (f a))
  dart x = liftA2 (:*:) (dart (fmap (\(y :*: _) -> y) <$> x)) (dart (fmap (\(_ :*: y) -> y) <$> x))

instance (Traversable f, Traversable g, Applicative h) => Dart (f :*: g) h where
  dart :: (f :*: g) ((f :*: g) (h a)) -> (f :*: g) (h ((f :*: g) a))
  dart ((:*:) x y) = fmap sequenceA x :*: fmap sequenceA y

instance (Dart [] g, Dart [] h) => Dart [] (g :+: h) where
  dart = concatMap pushSum
    where
      pushSum :: (Dart [] g, Dart [] h) => [(g :+: h) a] -> [(g :+: h) [a]]
      pushSum [] = []
      pushSum ~(x:xs) = case x of
                          ( L1 x') -> fmap L1 . dart . (:[]) $ x' : fmap (\(~(L1 y)) -> y) xs
                          ~(R1 x') -> fmap R1 . dart . (:[]) $ x' : fmap (\(~(R1 y)) -> y) xs

instance {-# OVERLAPPABLE #-} Dart f g => Dart (M1 i c f) g where
  dart = M1 . fmap (fmap M1) . dart . fmap unM1 . unM1

instance {-# OVERLAPPABLE #-} Dart f g => Dart f (M1 i c g) where
  dart = fmap M1 . dart . fmap (fmap unM1)

instance {-# OVERLAPPABLE #-} Dart f g => Dart (M1 i c f) (M1 i c g) where
  dart = M1 . fmap (M1 . fmap M1) . dart . unM1 . fmap (unM1 . fmap unM1)

instance Functor f => Dart (K1 i c) f where
  dart = K1 . unK1

instance Monad f => Dart f (K1 i c) where
  dart = fmap (K1 . unK1) . join

instance Functor f => Dart (Const c) f where
  dart = Const . getConst

instance Monad f => Dart f (Const c) where
  dart = fmap (Const . getConst) . join

instance Functor f => Dart U1 f where
  dart = const U1

instance Monad f => Dart f U1 where
  dart = fmap (const U1) . join

instance Functor f => Dart Identity f where
  dart = fmap (fmap return) . runIdentity

instance Functor f => Dart f Identity where
  dart = fmap (return . fmap runIdentity)

instance Functor f => Dart Par1 f where
  dart = fmap (fmap return) . unPar1

instance Functor f => Dart f Par1 where
  dart = fmap (return . fmap unPar1)

instance Dart f g => Dart (Rec1 f) g where
  dart = Rec1 . fmap (fmap Rec1) . dart . fmap unRec1 . unRec1

instance Dart f g => Dart f (Rec1 g) where
  dart = fmap Rec1 . dart . fmap (fmap unRec1)




class (Part f g, Dart f g) => Push f g where
  push :: f (g a) -> f (g (f a))
  push = dart . part

instance (Push f g, Push f h, Push g h, Pull f g, Pull g f) => Push (f :.: g) h

instance (Push f h, Push f g, Pull f g) => Push f (g :.: h)

instance (Push f h, Push g h) => Push (f :+: g) h

instance (Push f g, Push f h, Applicative f) => Push f (g :*: h)

instance (Push f h, Push g h, Traversable f, Traversable g, Applicative h) => Push (f :*: g) h

instance {-# OVERLAPPABLE #-} Push f g => Push (M1 i c f) g

instance {-# OVERLAPPABLE #-} Push f g => Push f (M1 i c g)

instance {-# OVERLAPPABLE #-} (Push f g, Dart (M1 i c f) (M1 j d g)) => Push (M1 i c f) (M1 j d g)

instance Functor f => Push (K1 i c) f

instance Monad f => Push f (K1 i c)

instance Functor f => Push (Const c) f

instance Monad f => Push f (Const c)

instance Functor f => Push U1 f

instance Monad f => Push f U1

instance Functor f => Push Identity f

instance Monad f => Push f Identity

instance Functor f => Push Par1 f

instance Monad f => Push f Par1

instance Push f g => Push (Rec1 f) g

instance Push f g => Push f (Rec1 g)

instance (Push [] g, Push [] h) => Push [] (g :+: h)


class (Functor f, Functor g) => Draw f g where
  draw :: f (g (f a)) -> f (f (g a))

instance (Dart f g, Draw f h, Draw g h, Draw f g, Push f g, Pull f h) => Draw (f :.: g) h where
  draw :: (f :.: g) (h ((f :.: g) a)) -> (f :.: g) ((f :.: g) (h a))
  draw =
    Comp1 .
    fmap (fmap Comp1) .
    dart .
    fmap (fmap (draw . unComp1)) .
    draw . fmap (Comp1 . fmap (fmap unComp1)) . unComp1

instance (Draw f h, Draw g h) => Draw (f :+: g) h where
  draw :: (f :+: g) (h ((f :+: g) a)) -> (f :+: g) ((f :+: g) (h a))
  draw (L1 x) = L1 . fmap L1 . draw . fmap (fmap (\(~(L1 y)) -> y)) $ x
  draw ~(R1 x) = R1 . fmap R1 . draw . fmap (fmap (\(~(R1 y)) -> y)) $ x

instance (Draw f g, Push f g, Pull f h) => Draw f (g :.: h) where
  draw :: f ((g :.: h) (f a)) -> f (f ((g :.: h) a))
  draw = fmap (fmap Comp1) . draw . fmap (fmap pull) . push . fmap unComp1


class (Functor f, Functor g) => Bond f g where
  bond :: f (f (g a)) -> f (g a)

instance (Pull f g, Pull g f, Functor h) => Bond (f :.: g) h where
  bond :: (f :.: g) ((f :.: g) (h a)) -> (f :.: g) (h a)
  bond = Comp1 . pull . fmap (pull . fmap unComp1) . unComp1

instance (Bond f h, Bond g h) => Bond (f :+: g) h where
  bond :: (f :+: g) ((f :+: g) (h a)) -> (f :+: g) (h a)
  bond (L1 x) = L1 . bond . fmap (\(~(L1 y)) -> y) $ x
  bond ~(R1 x) = R1 . bond . fmap (\(~(R1 y)) -> y) $ x

instance (Bond f g, Functor h) => Bond f (g :.: h) where
  bond :: f (f ((g :.: h) a)) -> f ((g :.: h) a)
  bond = fmap Comp1 . bond . fmap (fmap unComp1)


class (Draw f g, Bond f g) => Pull f g where
  pull :: f (g (f a)) -> f (g a)
  pull = bond . draw

instance (Pull f g, Pull f h, Pull g h, Pull g f, Pull f (g :.: h), Push f g) =>
         Pull (f :.: g) h

instance (Pull f h, Pull g h) => Pull (f :+: g) h

instance (Pull f g, Pull f h, Push f g) => Pull f (g :.: h)

-- f (g (f (g (h a)))) -> f (g (h (f (g a))))
-- pull . fmap (fmap pver) . push
-- fmap (fmap (fmap unComp1)) . push .
-- class (Functor f, Functor g, Functor (Pushed f g)) =>
--       Push f g
--   where
--   type Pushed f g :: * -> *
--   pushed :: Pushed f g a -> f (g a)
--   part :: f (g a) -> f (f (g a))
--   pver :: f (f (g a)) -> Pushed f g (f a)
--   push :: f (g a) -> Pushed f g (f a)
--   push = pver . part
-- class ( Functor f
--       , Generic1 f
--       , Functor g
--       , Generic1 g
--       , Functor (GPushed f g)
--       , Generic1 (GPushed f g)
--       ) =>
--       GPush f g
--   where
--   type GPushed f g :: * -> *
--   gpushed :: GPushed f g a -> f (g a)
--   gpart :: f (g a) -> f (f (g a))
--   gpver :: f (f (g a)) -> GPushed f g (f a)
-- absurdV1 :: V1 p -> a
-- absurdV1 x = x `seq` spin x
--   where
--     spin x = spin x
-- partPlus :: (f :.: h -> f :.: f :.: h) -> (g :.: h -> g :.: g :.: h) -> (f :+: g) :.: h -> ((f :.: f) :+: (g :.: g)) :.: h
-- partTimes :: (f :.: h -> f :.: f :.: h) -> (g :.: h -> g :.: g :.: h) -> (f :*: g) :.: h -> ((f :.: f) :*: (g :.: g)) :.: h
-- partCompose :: (f :.: h -> f :.: f :.: h) -> (g :.: h -> g :.: g :.: h) -> f :.: g :.: h -> f :.: f :.: g :.: g :.: h
-- (Part g h, Part f (Compose g h)) => Part (Compose f g) h where
--   part :: Compose f g (h a) -> Compose f g (Compose f g (h a))
--   part :: f (g (h a)) -> f (g (f (g (h a))))
-- fmap part :: f . g . h -> f . g . g . h
-- fmap pver :: f . g . g . h -> f . g . h . g
-- id :: f . g . h . g -> f . (g . h) . g
-- part :: f . (g . h) . g -> f . f . (g . h) . g
-- pver :: f . f . (g . h) . g -> f . (g . h) . f . g
-- id :: f . (g . h) . f . g -> f . g . h . f . g
-- pver = .
--   fmap (fmap (fmap pver)) :: (f . g) . (f . g) . h -> (f . g) . f . (h . g)
--   _ :: (f . g) . f . (h . g) -> (f . g) . h . (f . g)
--   pull . fmap (fmap pver) . push
-- instance (Functor g, Generic1 g) => GPush V1 g where
--   type GPushed V1 g = V1
--   gpushed = absurdV1
--   gpart = absurdV1
--   gpver = absurdV1
-- instance (Functor g, Generic1 g) => GPush U1 g where
--   type GPushed U1 g = U1
--   gpushed _ = U1
--   gpart _ = U1
--   gpver _ = U1
-- instance (Functor g, Generic1 g) => GPush Par1 g where
--   type GPushed Par1 g = g
--   gpushed :: g a -> Par1 (g a)
--   gpushed = Par1
--   gpart :: Par1 (g a) -> Par1 (Par1 (g a))
--   gpart = Par1
--   gpver :: Par1 (Par1 (g a)) -> g (Par1 a)
--   gpver = fmap Par1 . unPar1 . unPar1
-- instance GPush f g => GPush (M1 i c f) g where
--   type GPushed (M1 i c f) g = GPushed f g
--   gpushed = M1 . gpushed
--   gpart = M1 . fmap M1 . gpart . unM1
--   gpver = fmap M1 . gpver . fmap unM1 . unM1
-- instance (GPush f h, GPush g h) => GPush (f :*: g) h where
--   type GPushed (f :*: g) h = GPushed f h :*: GPushed g h
--   gpushed (x :*: y) = gpushed x :*: gpushed y
--   gpart (x :*: y) = _ (gpart x) (gpart y)
--   -- gpver (x :*: y) = _ x :*: _ y
newtype GPushedPlus f g h a = GPushedPlus
  { runGPushedPlus :: (GPushedPlus1 g h f :+: GPushedPlus1 h g f) a
  }

newtype GPushedPlus1 g h f a = GPushedPlus1
  { runGPushedPlus1 :: ((f :.: (g :+: h)) :+: (Maybe :.: g)) a
  } deriving (Generic, Generic1, Functor)

-- instance (GPush f g, GPush f h) => GPush f (g :+: h) where
--   type GPushed f (g :+: h) = GPushedPlus f g h
--   gpushed :: GPushed f (g :+: h) a -> f ((g :+: h) a)
--   gpushed (GPushedPlus x) =
--     case x of
--       L1 x' -> _ x'
--       ~(R1 x') -> _ x'
--   gpart :: f ((g :+: h) a) -> f (f ((g :+: h) a))
--   gpart =
--     fmap $ \x ->
--       case x of
--         L1 x' -> _ x'
--         ~(R1 x') -> _ x'
--   gpver :: f (f ((g :+: h) a)) -> GPushed f (g :+: h) (f a)
--   gpver = GPushedPlus . _
--   gpushed :: GPushed f (g :+: h) a -> f ((g :+: h) a)
--   gpart :: f ((g :+: h) a) -> f (f ((g :+: h) a))
--   gpver :: f (f ((g :+: h) a)) -> GPushed f (g :+: h) (f a)
-- data ((f :: k -> *) :+: (g :: k -> *)) (p :: k) infixr 5 Source#
-- L1 (f p)
-- R1 (g p)
newtype NonEmptyT f a = NonEmptyT
  { runNonEmptyT :: (a, f a)
  } deriving (Eq, Ord, Show, Read, Functor)

-- instance Functor f => Comonad (NonEmptyT f) where
--   extract ~(NonEmptyT (x, _)) = x
--   duplicate nt@(~(NonEmptyT (_, xs))) = NonEmptyT (nt, nt <$ xs)
data Pair a = Pair
  { pairFst :: !a
  , pairSnd :: !a
  } deriving (Eq, Ord, Show, Read, Functor)

instance Applicative Pair where
  pure = join Pair
  Pair fx fy <*> Pair x y = Pair (fx x) (fy y)

-- | `Push` one `Functor` through another.
--
-- The goal-function is `push`, which pushes a "layer" (`part`) of the outer `Functor` (@f@) "over" or "through" (`pver`) the inner `Functor` (@g@).
--
-- Some experiements and scratch-work proofs demonstrate that there are plenty of "exotic" cases,
-- where ignoring `part` and `pver` cause one's intuition about pushing one `Functor` through another to break down.
--
--
-- For example, `Either` has multiple, simple implementations:
--
--  1. Requiring `Monad`, even though `Applicative` would be enough for this implementation:
--
-- @
--   fmap (fmap return) :: (Functor f, Functor g, Monad m) => f         (g b) -> f         (g (m         b))
--   fmap (fmap return) :: (Functor f, Functor g)          => f         (g b) -> f         (g (Either a' b))
--   fmap (fmap return) ::             Functor g           => Either a' (g b) -> Either a' (g (Either a' b)) -- Note that t' does not have to be equivalent to t
--   push               ::             Functor g           => Either a  (g b) -> Either a  (g (Either a' b))
--   push               ::             Functor g           => Either a  (g b) -> Either a  (g (Either a  b))
-- @
--
--
--  2. Specializing (probably "too early") to `Right` since `return` seemed too abstract:
--
-- @
--   fmap (fmap Right) :: (Functor f, Functor g) => f         (g b) -> f         (g (Either a b))
--   fmap (fmap Right) ::             Functor g  => Either a' (g b) -> Either a' (g (Either a b))
--   fmap (fmap Right) ::             Functor g  => Either a  (g b) -> Either a  (g (Either a b))
--   push              ::             Functor g  => Either a  (g b) -> Either a  (g (Either a b))
-- @
--
--
--  3. Trying out specializing the left `fmap` first instead,
--  i.e. outermost specialization (right to left),
--  instead of the innermost specialization (left to right) found in 1. and 2.).
--
-- @
--   either Left (return . fmap return) :: (Functor g, Monad m) => Either a' (g a) -> Either a' (g (m        b))
--   either Left (Right  . fmap return) :: (Functor g, Monad m) => Either a' (g a) -> Either a' (g (m        b))
--   either Left (return . fmap return) ::  Functor g           => Either a' (g b) -> Either a' (g (Either a b))
--   either Left (return . fmap Right ) ::  Functor g           => Either a' (g b) -> Either a' (g (Either a b))
--   either Left (Right  . fmap Right ) ::  Functor g           => Either a' (g b) -> Either a' (g (Either a b))
--   either Left (Right  . fmap Right ) ::  Functor g           => Either a  (g b) -> Either a  (g (Either a b))
--   push                               ::  Functor g           => Either a  (g b) -> Either a  (g (Either a b))
-- @
--
--
-- Misc. notes:
--
-- @
--   t a
--   substructure (t a)
--
--   Ahh! For a (substructure -> Bool) partition into (True substructures, False substructures), this is just a push-through!
--
--   - What about for (substructure -> Int)?
--   - Or (substructure -> poset)?
--   - Or (substructure -> set)?
--
--   For a continuous, linear functor, (substructure -> [0,1::Real]) gives us a weighted filter on the functor.
--
--   In theory, this could be used to factor sounds:
--   - The continuous linear functor is a representation of the song's wave and the partition function is a recognizer
--    (it outputs the percent certainty that a wave clip is in a given class).
--
--   - Then, one can pull out (weighted) the matching and non-matching parts.
--     * By reducing the intensity of subsections in proportion, inverse proportion resp.
--       to the liklihood, we could derive an approximation of the removal, matching parts resp. of the matched sound(s).
-- @
notes :: ()
notes = ()


-- instance Push [] Maybe where
--   -- just drop the Nothings
--   type Pushed [] Maybe = PushedListMaybe
--   pushed :: Pushed [] Maybe a -> [a]
--   pushed (PushedList (Pure x)) = case x of
--                                    Nothing -> []
--                                    ~(Just x') -> [x']
--   pushed (PushedList (Free x)) = _ x
--   part :: [Maybe a] -> [NonEmptyT [] (Maybe a)]
--   part []      = []
--   part ~(x:xs) = toList . fmap (\(~(y :| ys)) -> NonEmptyT (y, ys)) . groupBy1 ((==) `on` isJust) $ x :| xs
--   pver :: [NonEmptyT [] (Maybe a)] -> Pushed [] Maybe (Maybe (NonEmptyT [] a))
--   pver [] = empty
--   pver [ne@(NonEmptyT (~(x, _)))] = case x of
--     Nothing -> empty
--     _       -> Pure . Just . fmap fromJust $ ne
--   pver ~(xs@(NonEmptyT (x, _)):ys:zs) = case x of
--     Nothing -> Free (Compose (Just (Pair Nothing (Just (fromJust <$> ys)), loopN zs)))
--     _       -> Free (Compose (Just (Pair (Just (fromJust <$> xs)) Nothing, loopJ zs)))
--     where
--       loopN [] = Pure Nothing
--       loopN [_] = Pure Nothing
--       loopN ~(_:qs:rs) = Free (Compose (Just (Pair Nothing (Just (fromJust <$> qs)), loopN rs)))
--       loopJ [] = Pure Nothing
--       loopJ [ws] = Pure . Just . fmap fromJust $ ws
--       loopJ ~(ws:_:rs) = Free (Compose (Just (Pair (Just (fromJust <$> ws)) Nothing, loopJ rs)))
--
-- instance Push (Cofree Maybe) Maybe where
--   part :: Cofree Maybe (Maybe a) -> Cofree Maybe (Cofree Maybe (Maybe a))
--   part xss@(x :< xs) = case xs of
--     Nothing -> xss :< Nothing
--     ~(Just xs') -> case x of
--       Nothing   -> case part xs' of
--         (ys@(Nothing :< _) :< zs) -> (x :< Just ys) :< zs
--         ys                        -> (x :< Nothing) :< Just ys
--       ~(Just _) -> case part xs' of
--         (ys@(Just _  :< _) :< zs) -> (x :< Just ys) :< zs
--         ys                        -> (x :< Nothing) :< Just ys
--   pver :: Cofree Maybe (Cofree Maybe (Maybe a)) -> Cofree Maybe (Maybe (Cofree Maybe a))
--   pver = fmap pver'
--     where pver' xs@(Nothing :< _) = Nothing
--           pver' xs                = Just $ (\(~(Just x)) -> x) <$> xs
--
-- instance Monad m => Push (Cofree (MaybeT m)) Maybe where
-- -- | `fmap` and `push`
-- pmap :: Push f g => (a -> g b) -> f a -> f (g (f b))
-- pmap f = push . fmap f
--
-- -- | `pmap` with a predicate
-- pmapP :: Push f Maybe => (a -> Bool) -> f a -> f (Maybe (f a))
-- pmapP p = pmap $ \x -> if p x
--                           then Just x
--                           else Nothing
--
-- instance Push NonEmpty Maybe where
--   type Pushed NonEmpty a = Free (Compose ((,) (Pair a)) Maybe) a
--   part :: NonEmpty (Maybe a) -> NonEmpty (NonEmptyT NonEmpty (Maybe a))
--   part = fmap _ . groupBy1 ((==) `on` isJust)
--   pver :: NonEmpty (NonEmpty (Maybe a)) -> Pushed NonEmpty (Maybe (NonEmpty a))
--   pver = _ -- fmap pver'
--     -- where
--     --   pver' (Nothing :| _) = Nothing
--     --   pver'  xs            = Just (fromJust <$> xs)
--
-- instance Monoid a => Push [] (Either a) where
--   part :: [Either a b] -> [[Either a b]]
--   part = groupBy ((==) `on` isLeft)
--   pver :: [[Either a b]] -> [Either a [b]]
--   pver = fmap pver'
--     where
--       pver' []            = undefined
--       pver' xs@(Left _:_) = Left (fold ((\(~(Left x)) ->x) <$> xs))
--       pver' xs            = Right ((\(~(Right x)) ->x) <$> xs)
--
-- instance Eq a => Push [] ((,) a) where
--   part :: [(a, b)] -> [[(a, b)]]
--   part = groupBy ((==) `on` fst)
--   pver = fmap pver'
--     where
--       pver' xs@(~((x, _):_)) = (x, snd <$> xs)


