{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S
import qualified Data.Function as F
import qualified Data.Char as C

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State f) = snd . f

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State f) = fst . f

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State $ \s -> (s, s)

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State $ const ((),s)

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f (State g) =
        State $ \s ->
          let
            (a, s') = g s
          in
            (f a, s')

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (a,)
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) (State f0) (State f1) =
    State $ \s ->
      let
        (g, s')  = f0 s
        (a, s'') = f1 s'
      in
        (g a, s'')

-- | Implement the `Monad` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
--
-- >>> runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
-- (10,16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) f (State g) =
    State $ \s ->
      let
        (a, s')  = g s
      in runState (f a) s'

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil     = pure Empty
findM p (x:.xs) = (\b -> if b then return $ Full x else findM p xs) =<< p x

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
-- turns out this version is eager, I wonder why.
firstRepeat ::
  forall a. Ord a =>
  List a
  -> Optional a
firstRepeat as =
  let
    go Nil     _   = Empty
    go (x:.xs) set = if S.member x set then Full x else go xs $ S.insert x set
    go2 = foldRight (\a (r, s) -> if S.member a s then (Full a, s) else (r, S.insert a s)) (Empty, S.empty)
  in
    fst $ go2 as -- S.empty

firstRepeat'' ::
  forall a. Ord a =>
  List a
  -> Optional a
firstRepeat'' as =
  let
    transition :: Ord a => a -> State (S.Set a) Bool
    transition a = State $ \s -> (S.member a s, S.insert a s)
    findSt = findM transition
  in
    eval (findSt as) S.empty

firstRepeat''' ::
  forall a. Ord a =>
  List a
  -> Optional a
firstRepeat''' ls =
  eval (findM p ls) S.empty
  where
    p x = get >>= \s -> put (S.insert x s) >>= const (pure (S.member x s))

removeRepeats ::
  forall a. Ord a =>
  List a
  -> List a
removeRepeats = F.fix $ \r as ->
  let
    rep :: Optional a
    rep = firstRepeat as
    as' :: List a
    as' = ((\a -> (\a' -> if a == a' then Nil else a' :. Nil) =<< as) <$> rep) ?? as
  in if as' /= as then r as' else as'


--- Experimentation on the test used for firstRepeat
--allRepeats ls =
--  let
--    (l, rx :. rs) = span (/= x) xs
--    (l2, rx' :. rs') = span (/= x) rs
--    l3 = (l ++ rx :. l2 ++ rs')
removeLastRepeat :: List Integer -> [Integer]
removeLastRepeat xs =
      case firstRepeat (xs :: List Integer) of
        Empty -> []
        Full x ->
          let
            (l, rx :. rs) = span (/= x) xs
            (l2, _) = span (/= x) rs
            l3 = hlist (l ++ rx :. l2)
          in
            l3
--- end of experimentation 

-- Tony's answer
firstRepeat' ::
  Ord a =>
  List a
  -> Optional a
firstRepeat' = listWithState findM S.member

listWithState ::
  Ord a1 =>
  ((a1 -> State (S.Set a1) a2)
  -> t
  -> State (S.Set a3) a)
  -> (a1 -> S.Set a1 -> a2)
  -> t
  -> a
listWithState f m x = eval (f (State . lift2 (lift2 (,)) m S.insert) x) S.empty


-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- filtering :: forall k a. Applicative k => (a -> k Bool) -> List a -> k (List a)
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct :: forall a. Ord a => List a -> List a
distinct as =
  let
    transition :: a -> State (S.Set a) Bool
    transition a = State $ \s -> (not (S.member a s), S.insert a s)
  in eval (filtering transition as) S.empty

--distinct' ::
--  Ord a =>
--  List a
--  -> List a
--distinct' =
--  listWithState filtering S.notMember


-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- produce ::  (a -> a) -> a -> List a
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy =
  let
    squared  = toInteger . sum . (join (*) . digitToInt <$>) . show'
    produced = produce squared
  in
    contains 1 . firstRepeat'' . produced
