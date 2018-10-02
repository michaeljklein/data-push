
module Scratch where

-- |
--
-- @
--  push :: f (Either a b) -> f (Either (f a) (f b))
--
--  we need to be able to:
--    f a -> f (StronglyConnectedComponent f a)
--
--  which is two steps:
--    isolate:
--      we need to isolate the SCC
--    constract:
--      we need to replace the SCC with a single node containing the SCC
--
--  what the end-functor construction does is replace the contraction with an equivalent operation: redefinition of data boundaries
--    with an added bonus: the boundaries are "free", and always result in an isomorphic type (proof?)
-- @
--
s1 :: ()
s1 = ()

-- |
--
-- @
--  An end-functor is a functor with the following properties:
--    It has an end value, whose type is solely dependent upon f:
--      type EndOf f :: *
--      endOf :: f a -> EndOf f
--      end :: EndOf f -> f a
--      endOf . end == id
--    Its end value may be replaced with a new one, as a result of a fold:
--      Bifunctor (End f)
--      type EndOf (End f e) = e
--      foldEnd :: Fold a (EndOf f -> e) -> f a -> End f e a
--      foldEndM :: Monad m => FoldM a (EndOf f -> e) -> f a -> m (End f e a)
--    Its middle values may be split by an end, resulting in a duplicate-like operation:
--      foldEnds :: (a -> Bool) -> f a -> FixEnd f a
--      foldEndsM :: Monad m => (a -> m Bool) -> f a -> m (FixEnd f a)
--
--      FoldM (Either (Left a)) b () -> FoldM (Either (Right b)) a () -> f (Either a b) -> EitherFlip (FixEnd2 f) a b
-- @
--
endFunctorDef :: ()
endFunctorDef = ()

-- |
--
-- @
--  what about:
--    newtype Stream a = Stream { runStream :: (a, Stream a) }
--  we can push it through Either:
--    Stream (Either a b) -> NonEmptyEnd (NonEmptyEnd .. b) a
--
--  newtype EitherFlip f a b = EitherFlip { getEitherFlip :: Either (f b a) (f a b) } deriving (Eq, Ord, Show, Functor)
--
--  newtype FixEnd f a = FixEnd { runFixEnd :: End f (FixEnd f a) a }
--
--  newtype FixEnd2 f a b = FixEnd2 { runFixEnd2 :: End f (FixEnd2 f b a) b }
--
--  newtype SomeIso a = SomeIso { runSomeIso :: (a -> b, b, b -> a) }
-- @
--
s2 :: ()
s2 = ()

-- -- Array

-- -- | We turn a 1xn array into a 2xn array, with the two options as the pairs.. hmm, maybe this should be:
-- -- EndArray i e a = EndArray (Strict e (Array i) a)
-- newtype EndArray i e a = EndArray { getEndArray :: Strict e (Array i) a } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- -- Graph


-- stronglyConnCompR
--         :: Ord key
--         => [(node, key, [key])]
--                 -- ^ The graph: a list of nodes uniquely identified by keys,
--                 -- with a list of keys of nodes this node has edges to.
--                 -- The out-list may contain keys that don't correspond to
--                 -- nodes of the graph; such edges are ignored.
--         -> [SCC (node, key, [key])]


-- data SCC vertex

-- Strongly connected component.

-- Constructors

-- AcyclicSCC vertex
-- A single vertex that is not in any cycle.

-- CyclicSCC [vertex]


-- flattenSCC :: SCC vertex -> [vertex]


-- contractSCC :: Ord key => SCC node -> [(node, key, [key])] -> [(node, key, [key])
-- contractSCC (AcyclicSCC _) ys = ys
-- contractSCC (CyclicSCC (x:xs)) ys = case find node
-- find all nodes
-- get all keys
-- replace all keys except that of the first node with that of the first node (no duplicates)


-- to push: filter the graph into Left and Right,
-- find the stronglyConnCompR of each,
-- then contractSCC for all of them, except, instead of deleting the subgraph, we want to replace the contents of the node at the only remaining vertex with the previous subgraph.
--   this will give us:
--     Graph (Either a b) -> Graph (Graph (Either a b)), where the contents of each subgraph is guaranteed to be homogenously Left or homogenously Right

-- forall f (g a)'s in f (f (g a)),
--   all $ (all =<< (==) . head) . fmap kindOf
--     :: (Foldable f, Data g) => f (f (g a)) -> Bool
--   where kindOf = data constructor class of, provided by Data
--         e.g. Left and Right

-- -- Etc.



-- push :: Set (Maybe a) -> Set (Maybe (Set a))
-- push x | Nothing `S.elem` x = [Nothing, Just $ (\(~(Just y)) -> y) <$> S.delete Nothing x]
--        | otherwise = [Just $ (\(~(Just y)) -> y) <$> x]


-- newtype PushedList a b = PushedList { getPushedList :: EndList (PushedList b a) b }

-- newtype Either2 f a b = Either2 { getEither2 :: Either (f b a) (f a b) }

-- push :: [Maybe a] -> Either2 PushedList (forall b. Maybe b) (Maybe [a])


-- newtype PushedBtree a b = PushedBtree { getPushedBtree :: EndBtree (PushedBtree b a) b }

-- push :: Btree (Maybe a) -> Either2 PushedBtree (forall t. Maybe t) (Maybe (Btree a))

-- class (Foldable f, Bifoldable (End f)) HasEnd f where
--   type End f e a :: *
--   end :: End f e a -> e
--   end = bifoldr1 const (flip const)

--   foldEnd :: Fold a e -> Fold (f a) (End f e a)
--   foldEndM :: Monad m => FoldM m a e -> FoldM m (f a) (End f e a)


{-

foldEndM :: FoldM m a e -> FoldM m (f a) (End f e a)

foldEither :: FoldM (Either b) a b -> Fold b c -> Fold a c
foldEither f g =
  fold with (f):
    Left -> cons onto g and restart
    Right -> cons onto g and end



fmap EndRec :: Monad m => FoldM m (f a) (End f (EndRec f c b) c) -> FoldM m (f a) (EndRec f b c)

-}


-- newtype EndRec f a b = EndRec { runEndRec :: End f (EndRec f b a) b }

-- class (Functor f, Functor (NonEmptyT f), Copointed (NonEmptyT f)) => HasNonEmpty f where
--   type NonEmptyT f :: * -> *
--   toNonEmptyT :: f a -> Maybe (NonEmptyT f a)
--   fromNonEmptyT :: NonEmptyT f a -> f a
--   extractNonEmptyT :: NonEmptyT f a -> a

-- newtype EitherFlip f a b = EitherFlip { getEitherFlip :: Either (f b a) (f a b) } deriving (Eq, Ord, Show, Functor)

-- newtype PushedSum f a b = PushedSum { getPushedSum :: EitherFlip (EndRec f) (NonEmptyT f a) (NonEmptyT f b) } deriving (Eq, Ord, Show, Functor)


-- class (HasNonEmpty f, HasEnd f) => PushSum f where
--   pushMaybe :: f (Maybe a) -> PushedSum f () a

--   pushEither :: f (Either a b) -> PushedSum f a b


