module Data.Unconsable
       ( class Unconsable
       , uncons

       , unconsableLength
       , checkUnconsableLaws

       , compareLength
       , compareLengths
       , isLongerThan
       , isShorterThan
       , isOfSameLength
       , longestTail
       , longestTail'
       , longerThan
       , shorterThan
       , hasLength
       , isSingleton
       , isEmpty
       )
where

import Data.Maybe (Maybe(..))
import Data.Array as A
import Data.Ord (class Ord, compare, (<))
import Data.Boolean (otherwise)
import Data.Semiring (class Semiring, add, one, zero)
import Data.Ordering (Ordering(..))
import Data.Eq ((==))
import Data.Function (const, (>>>))
import Data.List as L
import Data.List.Lazy as LL
import Data.CatQueue as CQ
import Data.CatList as CL
import Data.Tuple (Tuple(..))
import Data.Functor (map)
import Data.Either (Either (..))
import Data.Foldable (class Foldable, length)


-- | The only requirement for instances of this class is that
-- | `checkUnconsableLaws` must return `true` for all possible values.
-- |
-- | ```
-- | checkUnconsableLaws t =
-- |   unconsableLength t == length t :: Int
-- | ```
class Foldable t <= Unconsable t where
  uncons :: forall a. t a -> Maybe { head :: a, tail :: t a }


checkUnconsableLaws :: forall t a.
                       Unconsable t =>
                       t a -> Boolean
checkUnconsableLaws t =
  unconsableLength t == length t :: Int


-- | Equivalent to `length`, maybe except of stack safety.
-- |
-- | You may want to use this function instead of `length` if `foldr` for
-- | the particular data type you are using is not stack-safe.
unconsableLength :: forall t a num.
                    Unconsable t => Semiring num =>
                    t a -> num
unconsableLength = go zero
  where
    go n t = case uncons t of
      Just { tail } -> go (add one n) tail
      Nothing -> n


-- | Given `Foldable` instance `F` which is also an instance of
-- | `Unconsable`, for all `t :: F a` and for all `n`,
-- | `compareLength n t ＝ compare n (length t)`.
compareLength :: forall num t a .
              Semiring num => Ord num => Unconsable t =>
              num -> t a -> Ordering
compareLength n
  | n < zero = const LT
  | otherwise = go zero
  where
    go k t
      | otherwise = case uncons t of
        Just { head, tail } ->
          if n < k
          then LT
          else go (add one k) tail
        Nothing -> compare n k


-- | Given `Foldable` instances `F1` and `F2` which are also instances of
-- | `Unconsable`, for all `t1 :: F1 a` and `t2 :: F2 b`,
-- | `compareLengths t1 t2 ＝ compare (length t1) (length t2)`.
compareLengths :: forall t1 t2 a b.
                  Unconsable t1 => Unconsable t2 =>
                  t1 a -> t2 b -> Ordering
compareLengths t1 t2 =
  case uncons t1 of
    Just { head: head1, tail: tail1 } ->
      case uncons t2 of
        Just { head: head2, tail: tail2 } ->
          compareLengths tail1 tail2
        Nothing -> GT
    Nothing -> case uncons t2 of
      Just _ -> LT
      Nothing -> EQ


-- | Checks whether the first list is longer than the second.
isLongerThan :: forall t1 t2 a b.
                  Unconsable t1 => Unconsable t2 =>
                  t1 a -> t2 b -> Boolean
isLongerThan t1 t2 = compareLengths t1 t2 == GT


-- | Checks whether the first list is shorter than the second.
isShorterThan :: forall t1 t2 a b.
                  Unconsable t1 => Unconsable t2 =>
                  t1 a -> t2 b -> Boolean
isShorterThan t1 t2 = compareLengths t1 t2 == LT


-- | Checks whether two lists are of the same length.
isOfSameLength :: forall t1 t2 a b.
                  Unconsable t1 => Unconsable t2 =>
                  t1 a -> t2 b -> Boolean
isOfSameLength t1 t2 = compareLengths t1 t2 == EQ


-- | If two lists have equal lengths, returns nothing.
-- |
-- | If one of the lists is longer than the other, cuts the longer list at the
-- | index equal to the length of the shorter list and returns the rest of it
-- | (wrapped in `Either`, to allow passing lists of different types).
-- |
-- | You may want to use `longestTail'` instead, to avoid dealing with `Either`.
-- |
-- | ```purescript
-- | longestTail [] [] == Nothing
-- | longestTail [] [1,2] == (Just (Right [1,2]))
-- | longestTail [1,2,3,4] [1,2] == (Just (Left [3,4]))
-- | longestTail [1,2,3,4,5] [1,2,3,4,5] == Nothing
-- | ```
longestTail :: forall t1 t2 a b.
              Unconsable t1 => Unconsable t2 =>
              t1 a -> t2 b -> Maybe (Either (t1 a) (t2 b))
longestTail t1 t2 =
  case uncons t1 of
    Just { tail: tail1 } ->
      case uncons t2 of
        Just { tail: tail2 } ->
          longestTail tail1 tail2
        Nothing -> Just (Left t1)
    Nothing -> case uncons t2 of
      Just _ -> Just (Right t2)
      Nothing -> Nothing


-- | Less polymorphic version of `longestTail`.
longestTail' :: forall t a.
                Unconsable t =>
                t a -> t a -> Maybe (t a)
longestTail' t1 t2 =
  -- Would be nice to use `fanin` from profunctors here,
  -- but depending on it is too costly for us.
  map (case _ of
         Left x -> x
         Right x -> x) (longestTail t1 t2)


-- | Checks whether the list's length is strictly greater than the given number.
longerThan :: forall num t a .
              Semiring num => Ord num => Unconsable t =>
              t a -> num -> Boolean
longerThan t n = compareLength n t == LT


-- | Checks whether the list's length is strictly less than the given number.
shorterThan :: forall num t a .
              Semiring num => Ord num => Unconsable t =>
              t a -> num -> Boolean
shorterThan t n = compareLength n t == GT


-- | Checks whether the list's length is equal to the given number.
hasLength :: forall num t a .
             Semiring num => Ord num => Unconsable t =>
             t a -> num -> Boolean
hasLength t n = compareLength n t == EQ


-- | Checks whether the list's length is one.
isSingleton :: forall a t. Unconsable t => t a -> Boolean
isSingleton t = hasLength t 1


-- | Checks whether the list's length is zero.
isEmpty :: forall a t. Unconsable t => t a -> Boolean
isEmpty t = hasLength t 0



instance unconsableArray :: Unconsable Array where
  uncons = A.uncons

instance unconsableMaybe :: Unconsable Maybe where
  uncons (Just head) = Just { head, tail: Nothing }
  uncons Nothing = Nothing

instance unconsableList :: Unconsable L.List where
  uncons = L.uncons

instance unconsableLazyList :: Unconsable LL.List where
  uncons = LL.uncons

instance unconsableCatQueue :: Unconsable CQ.CatQueue where
  uncons = CQ.uncons >>> map fixTuple

instance unconsableCatList :: Unconsable CL.CatList where
  uncons = CL.uncons >>> map fixTuple


fixTuple :: forall head tail. Tuple head tail -> { head :: head, tail :: tail }
fixTuple (Tuple head tail) = { head, tail }
