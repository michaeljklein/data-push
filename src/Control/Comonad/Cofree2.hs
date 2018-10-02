{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Comonad.Cofree2 where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Cofree.Class
import Control.Comonad.Env
import Data.Bifunctor
import Data.Char
import Data.Functor.Classes
import Data.Monoid

-- | Example uses:
--
-- @
--  lines
--  comments
--  strings
--  matched: matched stack
--  spaces
--  alpha
--  numeric
--  symbols
--  build special symbols: ',` etc, into applications
--
--  add type for unresolved precidence problem including resoltions to applications
--
--  split into: var, app
--  split into: #:, ::, :#, incl. derived general resolutions (e.g. (.. (f x)) => f :: a -> b, x :: a)
-- @
--
exampleUses :: ()
exampleUses = ()


-- | Scan left, recording results in `Env`
scanlBicofree :: (c -> a -> c) -> c -> Bicofree Maybe a b -> Maybe (Cofree Maybe (Env c b))
scanlBicofree f x0 (Bicofree xs) = case xs of
                                     Left (y :<< ys) -> loopl (f x0 y) <$> ys
                                     ~(Right (y :<< ys)) -> return $ env x0 y :< (ys >>= loopr x0)
  where
    loopl z ~(w :<< ws) = env z w :< (ws >>= loopr z)
    loopr z ~(w :<< ws) = loopl (f z w) <$> ws

-- | Generate a `Bicofree` from `eitherCofreeMaybe2`, then `scanlBicofree` with `foldl` over the internal `Cofree`
pushScanlCofree :: (c -> a -> c)
                 -> c
                 -> Cofree Maybe (Either a b)
                 -> Maybe (Cofree Maybe (Env c (Cofree Maybe b)))
pushScanlCofree f x = scanlBicofree (foldl f) x . eitherCofreeMaybe2

-- Maybe (Cofree Maybe (CofreeT (Env c) Maybe b))

-- | Lines c a = Cofree Maybe (Env c a)
-- Cofree Maybe (Either a b) -> Maybe (Lines c (Cofree Maybe b))
linesCofree :: Enum c => Cofree Maybe (Either a b) -> Maybe (Cofree Maybe (Env c (Cofree Maybe b)))
linesCofree = pushScanlCofree (const . succ) (toEnum 1)

-- | Lefts are matches
eitherCofreeMaybeP ::
     (a -> Bool)
  -> Cofree Maybe a
  -> Cofree Maybe (Either a a)
eitherCofreeMaybeP p =
  fmap
    (\x ->
       if p x
         then Left x
         else return x)

-- | Lefts are newlines
eitherCofreeLines ::
     Comonad f
  => Cofree Maybe (f Char)
  -> Cofree Maybe (Either (f Char) (f Char))
eitherCofreeLines = eitherCofreeMaybeP ((== '\n') . extract)

-- | Collects spaces
eitherCofreeSpaces ::
     Comonad f
  => Cofree Maybe (f Char)
  -> Cofree Maybe (Either (f Char) (f Char))
eitherCofreeSpaces = eitherCofreeMaybeP (isSpace . extract)

-- | Begins when predicate matched, never ends
eitherCofreeMaybeBegin2 ::
     (a -> a -> Bool) -> Cofree Maybe a -> Cofree Maybe (Either a a)
eitherCofreeMaybeBegin2 begin ~(x :< xs) =
  case xs of
    Nothing -> return x :< empty
    ~(Just (y :< ys)) ->
      if begin x y
        then Left x :< return (Left y :< fmap (fmap Left) ys)
        else return x :< maybe (return (return y :< empty)) (return . loop y) ys
  where
    loop z ~(w :< ws) =
      if begin z w
        then Left w :< fmap (fmap Left) ws
        else return w :< fmap (loop w) ws

-- | Lefts are comments (ending the line)
eitherCofreeComment ::
     Comonad f
  => Cofree Maybe (f Char)
  -> Cofree Maybe (Either (f Char) (f Char))
eitherCofreeComment =
  eitherCofreeMaybeBegin2 (\c1 c2 -> extract c1 == '-' && extract c2 == '-')

-- | begin starts the Left, end ends it
eitherCofreeMaybeBeginEnd ::
     (a -> Bool) -> (a -> Bool) -> Cofree Maybe a -> Cofree Maybe (Either a a)
eitherCofreeMaybeBeginEnd begin end ~(x :< xs) =
  if begin x
    then Left x :< fmap loop xs
    else return x :< fmap (eitherCofreeMaybeBeginEnd begin end) xs
  where
    loop ~(y :< ys) =
      Left y :<
      if end y
        then eitherCofreeMaybeBeginEnd begin end <$> ys
        else loop <$> ys

-- | begin starts the Left, end ends it
eitherCofreeMaybeBeginEnd2 ::
     (a -> Bool)
  -> (a -> a -> Bool)
  -> Cofree Maybe a
  -> Cofree Maybe (Either a a)
eitherCofreeMaybeBeginEnd2 begin end ~(x :< xs) =
  if begin x
    then Left x :< fmap (loop x) xs
    else return x :< fmap (eitherCofreeMaybeBeginEnd2 begin end) xs
  where
    loop y ~(z :< zs) =
      Left z :<
      if end y z
        then eitherCofreeMaybeBeginEnd2 begin end <$> zs
        else loop z <$> zs

-- | Parses a string
eitherCofreeString ::
     Comonad f
  => Cofree Maybe (f Char)
  -> Cofree Maybe (Either (f Char) (f Char))
eitherCofreeString =
  eitherCofreeMaybeBeginEnd2
    ((== '"') . extract)
    (\c1 c2 -> extract c1 /= '\\' && extract c2 == '"')

-- | Use `Bicofree` to push through a representation of a non-empty list
eitherCofreeMaybe2 ::
     Cofree Maybe (Either a b)
  -> Bicofree Maybe (Cofree Maybe a) (Cofree Maybe b)
eitherCofreeMaybe2 ~(x :< xs) =
  Bicofree $
  case x of
    Left x' -> Left $ maybe (x' :< empty :<< empty) (loopl (x' :<)) xs
    ~(Right x') -> return $ maybe (x' :< empty :<< empty) (loopr (x' :<)) xs
  where
    loopl ::
         (Maybe (Cofree Maybe a) -> Cofree Maybe a)
      -> Cofree Maybe (Either a b)
      -> Cofree2 Maybe (Cofree Maybe b) (Cofree Maybe a)
    loopl ext ~(y :< ys) =
      case y of
        Left y' ->
          case ys of
            Nothing -> ext (return (y' :< empty)) :<< empty
            ~(Just ys') -> loopl (ext . return . (y' :<)) ys'
        ~(Right y') ->
          case ys of
            Nothing -> ext empty :<< return (y' :< empty :<< empty)
            ~(Just ys') -> ext empty :<< return (loopr (y' :<) ys')
    loopr ::
         (Maybe (Cofree Maybe b) -> Cofree Maybe b)
      -> Cofree Maybe (Either a b)
      -> Cofree2 Maybe (Cofree Maybe a) (Cofree Maybe b)
    loopr ext ~(y :< ys) =
      case y of
        Left y' ->
          case ys of
            Nothing -> ext empty :<< return (y' :< empty :<< empty)
            ~(Just ys') -> ext empty :<< return (loopl (y' :<) ys')
        ~(Right y') ->
          case ys of
            Nothing -> ext (return (y' :< empty)) :<< empty
            ~(Just ys') -> loopr (ext . return . (y' :<)) ys'

-- eitherCofree2 ::
--      Functor f => Cofree f (Either a b) -> Bicofree f (Cofree f a) (Cofree f b)
-- eitherCofree2 ~(x :< xs) = Bicofree $ case x of
--                                         Left    x'  -> Left  $ _ x' xs
--                                         ~(Right x') -> Right $ _ x' xs
--   where
--     loopl :: a -> f (Cofree f (Either a b)) -> Cofree2 f (Cofree f b) (Cofree f a)
--     loopl y ys = liftA2 (:<<) _ _
--     -- loopr :: b -> f (Cofree f (Either a b)) -> Cofree2 f (Cofree f a) (Cofree f b)
--     -- loopr ext

-- | See `fromCofree2`
fromBicofree :: Functor f => Bicofree f a a -> Cofree f a
fromBicofree = either fromCofree2 fromCofree2 . runBicofree

-- | `Cofree2`, when we don't know whether @a@ or @b@ occurs first
newtype Bicofree f a b = Bicofree
  { runBicofree :: Either (Cofree2 f b a) (Cofree2 f a b)
  }

instance Functor f => Bifunctor (Bicofree f) where
  bimap f g (Bicofree x) = Bicofree (bimap (bimap g f) (bimap f g) x)

instance Functor f => Functor (Bicofree f a) where
  fmap = second

-- | Rewriting @(`:<<`) -> (`:<`)@ gives `Cofree`
fromCofree2 :: Functor f => Cofree2 f a a -> Cofree f a
fromCofree2 ~(x :<< xs) = x :< fmap fromCofree2 xs

-- | Unwrap two layers of a `Cofree2` by dropping their values
unwrap2 :: Functor f => Cofree2 f a b -> f (f (Cofree2 f a b))
unwrap2 ~(_ :<< xs) = (\(~(_ :<< ys)) -> ys) <$> xs


infixr 4 :<<

data Cofree2 f a b where
  (:<<) :: b -> f (Cofree2 f b a) -> Cofree2 f a b

instance Functor f => Bifunctor (Cofree2 f) where
  bimap f g (x :<< xs) = g x :<< fmap (bimap g f) xs

instance Functor f => Functor (Cofree2 f a) where
  fmap = second

instance Functor f => Comonad (Cofree2 f a) where
  extract (x :<< _) = x
  duplicate xss@(_ :<< xs) =
    xss :<< fmap (\(y :<< ys) -> y :<< fmap duplicate ys) xs

instance Eq1 f => Eq2 (Cofree2 f) where
  liftEq2 eq1 eq2 (x :<< xs) (y :<< ys) =
    eq2 x y && liftEq (liftEq2 eq2 eq1) xs ys

instance Ord1 f => Ord2 (Cofree2 f) where
  liftCompare2 ord1 ord2 (x :<< xs) (y :<< ys) =
    ord2 x y <> liftCompare (liftCompare2 ord2 ord1) xs ys

-- instance Show1 f => Show2 (Cofree2 f)
-- instance Read1 f => Read2 (Cofree2 f)

