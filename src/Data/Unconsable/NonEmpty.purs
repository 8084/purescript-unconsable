module Data.Unconsable.NonEmpty where

import Data.List.NonEmpty as NL
import Data.List as L
import Data.List.Lazy.NonEmpty as NLL
import Data.List.Lazy as LL
import Data.Array.NonEmpty as A
import Data.NonEmpty


-- | The user should interpret `Unconsable1 s t` constraint as
-- | "`s` is a non-empty container which can be converted to a possibly empty
-- | container `t`".
class Unconsable1 s t | s -> t where
  uncons1 :: forall e. s e -> { head :: e, tail :: t e }


instance unconsable1Array :: Unconsable1 A.NonEmptyArray Array where
  uncons1 = A.uncons

instance unconsable1List :: Unconsable1 NL.NonEmptyList L.List where
  uncons1 = NL.uncons

instance unconsable1LazyList :: Unconsable1 NLL.NonEmptyList LL.List where
  uncons1 = NLL.uncons

instance unconsable1NonEmpty :: Unconsable1 (NonEmpty t) t where
  uncons1 (NonEmpty head tail) = { head, tail }
