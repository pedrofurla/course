{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor k of (a produced value `a`, and a resulting state `s`).
newtype StateT s k a =
  StateT {
    runStateT ::
      s
      -> k (a, s)
  }

-- | Implement the `Functor` instance for @StateT s k@ given a @Functor k@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor k => Functor (StateT s k) where
  (<$>) ::
    (a -> b)
    -> StateT s k a
    -> StateT s k b
  f <$> StateT t = StateT $ (first f <$>) . t

-- | Implement the `Applicative` instance for @StateT s k@ given a @Monad k@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) <*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) <*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad k => Applicative (StateT s k) where
  pure ::
    a
    -> StateT s k a
  pure a = StateT $ pure . (a,)
  (<*>) :: forall a b.
    StateT s k (a -> b)
    -> StateT s k a
    -> StateT s k b
--  StateT f <*> StateT t = StateT $ \s -> (\(f', s') -> first f' <$> t s') =<< f s
  StateT f <*> StateT t = StateT $ ((\(f', s) -> first f' <$> t s) =<<) . f


-- | Implement the `Monad` instance for @StateT s k@ given a @Monad k@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad k => Monad (StateT s k) where
  (=<<) :: forall a b.
    (a -> StateT s k b)
    -> StateT s k a
    -> StateT s k b
  f =<< StateT t =
    StateT $ (uncurry (runStateT . f) =<<) . t
    -- StateT $ \s -> uncurry (runStateT . f) =<< t s

{-
  f =<< StateT t =
    StateT $ \s ->
      let
         g :: a -> s -> k (b, s)
         g = runStateT . f
         z :: k (a, s)
         z = t s
      in (\(a, s') -> g a s') =<< t s
-}



-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f =
  StateT $ ExactlyOne . f

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
-- runState' (StateT f) s = t where ExactlyOne t = f s
runState' (StateT f) = runExactlyOne . f

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT ::
  Functor k =>
  StateT s k a
  -> s
  -> k s
execT (StateT f) s = snd <$> f s

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' ::
  State' s a
  -> s
  -> s
exec' st = snd . runState' st

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT ::
  Functor k =>
  StateT s k a
  -> s
  -> k a
evalT (StateT f) s = fst <$> f s

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' ::
  State' s a
  -> s
  -> a
eval' st = fst . runState' st

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative k =>
  StateT s k s
getT = StateT $ \s -> pure (s,s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative k =>
  s
  -> StateT s k ()
putT s = StateT $ const $ pure ((),s)

-- | Remove all duplicate elements in a `List`.
--
-- filtering :: Applicative k => (a -> k Bool) -> List a -> k (List a)
-- type State' s a = StateT s ExactlyOne a
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  Ord a =>
  List a
  -> List a
distinct' as = eval' (filtering transition as) S.empty
  where
    transition a = state' $ \s -> (not $ S.member a s, S.insert a s)


-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF as = evalT (filtering transition as) S.empty
  where
    transition a = StateT $ \s ->
      if a > 100 then Empty else Full (not $ S.member a s, S.insert a s)


-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT k a =
  OptionalT {
    runOptionalT ::
      k (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT k` given a Functor k.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor k => Functor (OptionalT k) where
  (<$>) :: forall a b.
    (a -> b)
    -> OptionalT k a
    -> OptionalT k b
  (<$>) f = OptionalT . ((f <$>) <$>) . runOptionalT
--  (<$>) f (OptionalT a) = OptionalT $ (f <$>) <$> a

-- Doesn't work  'cause Functor k doesn't imply Applicative k:
-- (<$>) f = OptionalT . lift2 (<*>) f'  . runOptionalT
--    where
--        f' :: k (Optional (a -> b))
--        f' =  pure $ pure f

{-

λ> :t  lift2 (<*>)
lift2 (<*>)
  :: forall {k1 :: * -> *} {k2 :: * -> *} {a} {b}.
     (Applicative k1, Applicative k2) =>
     k1 (k2 (a -> b)) -> k1 (k2 a) -> k1 (k2 b)

-}

-- | Implement the `Applicative` instance for `OptionalT k` given a Monad k.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad k => Applicative (OptionalT k) where
  pure ::
    a
    -> OptionalT k a
  pure = OptionalT . pure . pure

  (<*>) ::
    OptionalT k (a -> b)
    -> OptionalT k a
    -> OptionalT k b
--  (<*>) (OptionalT f) (OptionalT a) = OptionalT $ lift2 (<*>) f a
--  (<*>) (OptionalT f) = OptionalT . lift2 (<*>) f . runOptionalT    -- doesn't work
  OptionalT f <*> OptionalT a =
    OptionalT (f >>= optional (\f' -> (f' <$>) <$> a) (pure Empty))   -- Tony's answer

wrongOptionalTApply :: Applicative k =>
                       OptionalT k (a1 -> a2) -> OptionalT k a1 -> OptionalT k a2
wrongOptionalTApply (OptionalT f) = OptionalT . lift2 (<*>) f . runOptionalT

-- | Implement the `Monad` instance for `OptionalT k` given a Monad k.
--
-- onFull :: Applicative k => (t -> k (Optional a)) -> Optional t -> k (Optional a)
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad k => Monad (OptionalT k) where
  (=<<) :: forall a b.
    (a -> OptionalT k b)
    -> OptionalT k a
    -> OptionalT k b
  (=<<) f (OptionalT a) = OptionalT $ id =<< onFull (runOptionalT . f) <$> a
--  (=<<) f (OptionalT a) = error ""
--    where
--       l :: Monad m => (a -> m (Optional b)) -> a -> m (Optional (a -> b))
--       l m a = (const <$>) <$> m a

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) ::
    (a -> b)
    -> Logger l a
    -> Logger l b
  f <$> (Logger ls a) = Logger ls (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure ::
    a
    -> Logger l a
  pure = Logger Nil

  (<*>) ::
    Logger l (a -> b)
    -> Logger l a
    -> Logger l b
  (<*>) (Logger ls0 f) (Logger ls1 a) = Logger (ls0 ++ ls1) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) ::
    (a -> Logger l b)
    -> Logger l a
    -> Logger l b
  f =<< Logger ls0 a = Logger (ls0 ++ ls1) b
    where Logger ls1 b = f a

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 = Logger . (:. Nil)

log0 :: forall a l. a -> Logger l a
log0 = Logger Nil

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
--
-- StateT    s k a = StateT    s -> k (a, s)
-- OptionalT k a   = OptionalT (k (Optional a))
-- Logger s a      = Logger    (List l) a
distinctG ::
  forall a. (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG as = runOptionalT v
  where
    v :: OptionalT (Logger Chars) (List a)
    v = evalT (filtering transition as) S.empty
    log :: a -> OptionalT (Logger Chars) a
    log n | n > 100   = OptionalT $ log1 ("aborting > 100: " ++ (listh . show) n) Empty
          | even n  =   OptionalT $ log1 ("even number: "    ++ (listh . show) n) $ Full n
          | otherwise = OptionalT $ log0 $ Full n
    transition :: a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
    transition a = StateT $ \s -> log a >>= (\n -> pure (not (S.member n s), S.insert n s))

onFull ::
  Applicative k =>
  (t -> k (Optional a))
  -> Optional t
  -> k (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
