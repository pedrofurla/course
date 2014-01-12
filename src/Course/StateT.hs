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
distinctF as =
  let
    -- getT :: Monad f => StateT s f s
    -- putT :: Monad f => s -> StateT s f ()
    p :: (Ord b, Num b) ⇒ b → StateT (S.Set b) Optional Bool
    p x = (\set ->
            (const $
             if x P./= 100
             then pure (S.member x set)
             else StateT (\_ -> Empty)) =<< putT (S.insert x set)) =<< getT
  in
   evalT (filtering p as) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
instance Functor f => Functor (OptionalT f) where
  g <$> (OptionalT f) = OptionalT ( (g <$>) <$> f)

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
instance Apply f => Apply (OptionalT f) where
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  -- (<*>) :: f Optional (a -> b) -> f Optional a -> f Optional b
  (OptionalT z) <*> (OptionalT x) =
    OptionalT
    (
      let 
        app ∷ Optional (a -> b) -> (Optional a -> Optional b)
        app oab = (oab <*>)
        --map' ∷ f (Optional a -> Optional b)
        map' = (app <$> z)
      in
       map' <*> x)
    
-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure = OptionalT . (pure . pure)

-- | Implement the `Bind` instance for `OptionalT f` given a Bind f.
instance Bind f => Bind (OptionalT f) where
  -- (a → f b) → f a → f b
  -- =<< :: (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  g =<< OptionalT z =
    OptionalT (
      let 
        fempty = const Empty <$> z
        bindf = runOptionalT . g
      in
       (\x -> -- TODO there must be a better way
         case x of 
           Full a -> bindf a
           Empty -> fempty) =<< z
      )
   

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger`.
instance Functor (Logger l) where
  f <$> (Logger xs a) = Logger xs (f a)

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  (Logger ls f) <*> (Logger rs a) = Logger (ls ++ rs) (f a)
    
-- | Implement the `Applicative` instance for `Logger`.
instance Applicative (Logger l) where
  pure a = Logger Nil a

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
instance Bind (Logger l) where
  f =<< (Logger ls a) =
    let
      (Logger ls2 a2) = f a
    in
     Logger (ls2 ++ ls) a2

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 l a = Logger (pure l) a

appendLog ∷
  l
  → (Logger l a)
  → Logger l a
appendLog l (Logger ls a) = Logger (l :. ls) a 

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).

type OLoggerT b = (OptionalT (Logger Chars) b)
type OLoggerT' = OptionalT (Logger Chars)
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG as =
  let
    -- StateT s f a = StateT ( s → f (a, s) )
    -- OptionalT f a = f (Optional a)
    -- Logger l a = Logger (List l) a
    logAbort x o =
      o >>= (\_ → OptionalT $ log1 ("aborting > 100: " ++ (listh $ show x)) $ Empty)
    logEven  x o =
      o >>= (\v -> OptionalT $ log1 ("even number: " ++  (listh $ show x)) $ Full v)
    p :: (Show b, Integral b) ⇒ b → StateT (S.Set b) OLoggerT' Bool
    p x = StateT (
      \s →
      let
        s2 = S.insert x s
        member = S.member x s
        ifEven = if even x  then logEven x else id
        ifg100 = if x > 100 then logAbort x else id
      in
        ifg100 $ ifEven $ pure (not member, s2)
      )
    filt = runStateT (filtering p as) S.empty
    (OptionalT(Logger logs oas)) = filt
  in
    Logger logs (fst <$> oas)
