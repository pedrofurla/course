{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
instance Functor f => Functor (StateT s f) where
  g <$> (StateT z) =
    StateT (\x →
             let f = z x in
             (\(a ,s2) → (g a, s2)) <$> f)

-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
instance Bind f => Apply (StateT s f) where
  -- Apply f ⇒ (<*>) :: f (a -> b) -> f a -> f b
  -- Apply StateT s f ⇒ (<*>) ∷ StateT s f (a → b) → StateT s f a → StateT s f b
  -- (=<<) :: (a -> f b) -> f a -> f b
  (StateT g) <*> (StateT z) =
    StateT (\s →
             let               
               f = z s -- f (a, s)
               -- g ∷ s → f (a → b, s)
               -- (g s2) ∷ f (a → b, s) )
               fstmap ∷ (a → b, s) → a → (b, s)
               fstmap (ab, s') a = (ab a, s')
               --outside ∷ (a, s) → f (b, s)
               outside (a, s') = (flip fstmap $ a) <$> (g s')
             in
              outside =<< f)
              --(\(a,s2) → (\(ab,s3) → (ab a,s3)) <$> (g s2)) =<< f ) -- TODO this is horrible, can use kleisli composition here?

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
instance Monad f => Applicative (StateT s f) where
  pure a = StateT (\s → pure (a, s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
instance Monad f => Bind (StateT s f) where
  g =<< (StateT z) =
    StateT (\s →
             let
               f = z s -- f (a, s)
               -- bindfst ∷ (a, s) → f (b, s)
               bindfst (a, s') = let (StateT k) = (g a) in k s'
             in
              f >>= bindfst)

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a = StateT s Id a

-- | Provide a constructor for `State'` values.
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT (\s → Id $ f s)

-- | Provide an unwrapper for `State'` values.
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT g) s = runId $ g s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT g) s = snd <$> g s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st s = runId $ execT st s 

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT g) s = fst <$> g s 
  
-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st s = runId $ evalT st s

-- | A `StateT` where the state also distributes into the produced value.
getT ::
  Monad f =>
  StateT s f s
getT = StateT $ pure . dup where dup x = (x,x)

-- | A `StateT` where the resulting state is seeded with the given value.
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT s = StateT $ pure . (const ((),s))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' as =
  let
    p :: (Ord b, Num b) ⇒ b → State' (S.Set b) Bool
    p x = (\set -> (const $ pure (S.member x set)) =<< putT (S.insert x set)) =<< getT
  in
  -- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)
  -- let Id((xs,_)) = runStateT (filtering p as) S.empty in xs
   eval' (filtering p as) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF =
  let
    -- getT :: Monad f => StateT s f s
    -- putT :: Monad f => s -> StateT s f ()
    p :: (Ord b, Num b) ⇒ b → StateT (S.Set b) Optional Bool
    p x = (\set ->
            (const $
             if x P./= 100
             then pure (S.member x set)
             else pure False) =<< putT (S.insert x set)) =<< getT
  in
   error "todo"

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
instance Functor f => Functor (OptionalT f) where
  (<$>) =
    error "todo"

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
instance Apply f => Apply (OptionalT f) where
  (<*>) =
    error "todo"

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure =
    error "todo"

-- | Implement the `Bind` instance for `OptionalT f` given a Bind f.
instance Bind f => Bind (OptionalT f) where
  (=<<) =
    error "todo"

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger`.
instance Functor (Logger l) where
  (<$>) =
    error "todo"

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  (<*>) =
    error "todo"

-- | Implement the `Applicative` instance for `Logger`.
instance Applicative (Logger l) where
  pure =
    error "todo"

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
instance Bind (Logger l) where
  (=<<) =
    error "todo"

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 =
  error "todo"

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG =
  error "todo"
