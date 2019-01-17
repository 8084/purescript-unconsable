module Data.Unconsable0 where

import Data.Maybe


class Unconsable0 t a | t -> a where
  uncons0 :: t -> Maybe { head :: a, tail :: t }


instance unconsable0Maybe :: Unconsable0 t a => Unconsable0 (Maybe t) a where
  uncons0 (Just x) = case uncons0 x of
    Just { head, tail } -> Just { head, tail: Just tail }
    Nothing -> Nothing
  uncons0 Nothing = Nothing
