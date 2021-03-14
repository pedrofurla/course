{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Comonad where

import Course.Core
import Course.ExactlyOne
import Course.Extend
import Course.List

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend k => Comonad k where
  copure :: k a -> a

-- pure :: a -> k a

-- | Implement the @Comonad@ instance for @ExactlyOne@.
--
-- >>> copure (ExactlyOne 7)
-- 7
instance Comonad ExactlyOne where
  copure :: ExactlyOne a -> a
  copure (ExactlyOne a) = a

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$$> ExactlyOne 7
-- ExactlyOne 17

-- >>> (+10) <$$> (1 :. 2 :. 3 :. Nil)
(<$$>) :: Comonad k => (a -> b) -> k a -> k b
(<$$>) f ka = (f . copure) <<= ka

-- instance Comonad List where
--   copure :: List a -> a
--   copure as = undefined -- _todo
