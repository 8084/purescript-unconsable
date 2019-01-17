module Data.Unconsable where

import Data.Maybe
import Data.Array as A


class Unconsable t where
  uncons :: forall a. t a -> Maybe { head :: a, tail :: (t a) }


instance unconsableArray :: Unconsable Array where
  uncons = A.uncons

instance unconsableMaybe :: Unconsable Maybe where
  uncons (Just a) = Just { head: a, tail: Nothing }
  uncons Nothing = Nothing
