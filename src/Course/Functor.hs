{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.List
import Course.Optional
import qualified Prelude as P (fmap)

-- | All instances of the `Functor` type-class must satisfy two laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor k where
  -- Pronounced, eff-map.
  (<$>) :: (a -> b) -> k a -> k b

infixl 4 <$>

-- data type that is not a functor
-- newtype T a = T (a -> Int)

-- Cannot create a `T b` below to implement fmap
-- instance Functor T where
--   (<$>) :: (a -> b) -> T a -> T b
--   (<$>) f (T a) = _t

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  (<$>) :: (a -> b) -> ExactlyOne a -> ExactlyOne b
  (<$>) f (ExactlyOne a) = ExactlyOne (f a)

-- Law of Identity:
-- id <$> ExactlyOne 5 == ExactlyOne 5

-- Law of Composition:
-- ((*2) . (+3) <$> (ExactlyOne 5)) == ((*2) <$> ((+3) <$> (ExactlyOne 5)))

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) :: (a -> b) -> List a -> List b
  (<$>) = map

-- id
-- (id <$> (1 :. 2 :. Nil)) == (1 :. 2 :. Nil)

-- composition
-- ((*2) . (+3) <$> (1 :. 2 :. Nil)) == ((*2) <$> ((+3) <$> (1 :. 2 :. Nil)))

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) :: (a -> b) -> Optional a -> Optional b
  (<$>) = mapOptional

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) :: (a -> b) -> (t -> a) -> t -> b
  (<$>) = (.)

-- ra :: t -> a
-- f :: a -> b
-- ra . f :: t -> a -> b

-- id
-- (id <$> f) == f
-- (id . f) == f

-- composition
-- f . g <$> h == f <$> g <$> h
-- f . g . h == f . g . h

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) :: Functor k => a -> k b -> k a
(<$) a kb = const a <$> kb

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void :: Functor k => k a -> k ()
void ka = () <$ ka

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
