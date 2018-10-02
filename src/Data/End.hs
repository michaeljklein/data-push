module Data.End where

class HasEnd f where
  end :: f e a -> e

instance HasEnd (,) where
  end = fst


-- data Strict f a = Strict { getStrict :: !(f a) } deriving (Eq, Ord, Show, Functor, Generic, Generic1)

-- data EndStrict f e a = EndStrict { getEndStrict :: !(Strict f a)
--                                  , endStrictEnd :: !e
--                                  } deriving (Eq, Ord, Show, Functor, Generic, Generic1)



-- newtype PairEnded p a b = Either (p (Maybe a) (a, b)) (p (Maybe b) (b, a))

-- instance (Functor f, Bifunctor p) => HasEnd f p | f -> p where
--   end :: p e a -> e

--   foldEnd :: Fold a e -> f a -> p e a

--   foldEndM :: Monad m => FoldM m a e -> f a -> m (p e a)

--   fromEnd :: p e a -> f a

--   pushEnded :: f (Either a b) -> PairEnded p (f a) (f b)

--   pushEndedMaybe :: f (Maybe a) -> PairEnded p (f ()) (f a)


-- instance HasEnd (Strict f) (EndStrict f) where
--   end = endStrictEnd
--   foldEnd f xs = EndStrict xs (fold f xs)
--   foldEndM f xs = EndStrict xs <$> foldM f xs
--   fromEnd = getEndStrict




-- instance HasEnd [] EndList

-- instance HasEnd Tree EndTree

-- instance HasEnd Forest EndForest

-- instance HasEnd NonEmpty EndNonEmpty

-- instance HasEnd Btree EndBtree

-- instance HasEnd Btree1 EndBtree1



-- -- Fold a e -> [a] -> EndList e a
-- -- FoldM m a e -> [a] -> m (EndList e a)


-- -- Push maps:

-- instance (Ord k, Functor f) => Push (Map k) f where
--   part :: Map k (f a) -> Map k (Map k (f a))
--   {-# INLINE part #-}
--   part = M.mapWithKey M.singleton

--   dart :: Map k (Map k (f a)) -> Map k (f (Map k a))
--   {-# INLINE dart #-}
--   dart = M.mapWithKey $ \k x -> M.singleton k <$> fromSingleton x

-- import qualified Data.Map.Internal as IM

-- fromSingletonMap :: Map k a -> a
-- {-# INLINE fromSingletonMap #-}
-- fromSingletonMap ~(MI.Bin 1 _ x MI.Tip MI.Tip) = x


-- instance Functor f => Push IntMap f where
--   part :: IntMap (f a) -> IntMap (IntMap (f a))
--   {-# INLINE part #-}
--   part = IM.mapWithKey IM.singleton

--   dart :: IntMap (IntMap (f a)) -> IntMap (f (IntMap a))
--   {-# INLINE dart #-}
--   dart = IM.mapWithKey $ \k x -> IM.singleton k <$> fromSingletonIntMap x


-- import qualified Data.IntMap.Internal as IMI

-- fromSingletonIntMap :: IntMap a -> a
-- {-# INLINE fromSingletonIntMap #-}
-- fromSingletonIntMap ~(IMI.Tip _ x) = x


-- -- Push Representable:

-- instance (Representable f, Semigroup (Key f), Functor g) => Push (Rep f) g where
--   part :: Rep f (g a) -> Rep f (Rep f (g a))
--   part = duplicatedRep

--   dart :: Rep f (Rep f (g a)) -> Rep f (g (Rep f a))
--   dart = fmap return . join

-- instance Representable g => Part [] (Rep g) where
--   part = fmap (: [])

-- instance Representable g => Part Maybe (Rep g) where
--   part = fmap Just

-- instance Representable g => Part (Either e) (Rep g) where
--   part = fmap Right


-- instance (Functor f, Representable g) => Dart f (Rep g) where
--   dart :: f (f (Rep g a)) -> f (Rep g (f b))
--   dart = fmap distribute


-- -- EndSeq, I'm pretty excited

-- Seq a -> EndSeq e a

-- data EndFingerTree e a = EndEmptyT e
--                        | EndSingle a
--                        | EndDeep !Int !(Digit a) (EndFingerTree e (Node a)) !(Digit a)

-- newtype Seq a = Seq (FingerTree (Elem a))

-- newtype EndSeq e a = EndSeq (EndFingerTree e (Elem a))



-- -- | Things you can push a sum through
-- class PushSum f where
--   pushMaybe :: f (Maybe a) -> Either2 (Pushed f) (forall t. Maybe t) (Maybe (Pushed1 f a))

--   pushEither :: f (Either a b) -> Either2 (Pushed f) (forall t. Either a t) (forall t. Either t b)

-- class PushSum f where
--   type PushedSumEnd f e a :: *
--   pushMaybe :: f (Maybe a) -> EitherFlip (PushedSumEnd f)

-- newtype SumEnd f a b = SumEnd { getSumEnd :: End f (SumEnd f b a) b } deriving (Eq, Ord, Show, Functor)




