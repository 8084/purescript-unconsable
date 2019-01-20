module Test.Main where

import Data.Unconsable

import Prelude
import Test.Assert
import Effect (Effect, forE)
import Data.Foldable (for_, length)
import Data.Maybe
import Data.Either
import Data.Array
import Effect.Console

import Data.List as L
import Data.List.Lazy as LL
import Data.CatQueue as CQ
import Data.CatList as CL


main :: Effect Unit
main = do
  assertTrue  $ isSingleton [1]
  assertFalse $ isSingleton []
  assertFalse $ isSingleton [1,2]

  assertFalse $ [] `longerThan` 1
  assertTrue  $ [] `longerThan` (-1)
  assertFalse $ [] `longerThan` 0
  assertFalse $ [] `longerThan` 1
  assertTrue  $ [1,2,3] `longerThan` 1
  assertTrue  $ [1,2,3] `longerThan` 2
  assertFalse $ [1,2,3] `longerThan` 3


  assertTrue  $ [] `shorterThan` 1
  assertFalse $ [] `shorterThan` (-1)
  assertFalse $ [] `shorterThan` 0
  assertTrue  $ [] `shorterThan` 1
  assertFalse $ [1,2,3] `shorterThan` 1
  assertFalse $ [1,2,3] `shorterThan` 2
  assertFalse $ [1,2,3] `shorterThan` 3
  assertTrue  $ [1,2,3] `shorterThan` 4

  assertEqual { expected: EQ
              , actual: compareLength 1 [1] }
  assertEqual { expected: GT
              , actual: compareLength 1 [] }
  assertEqual { expected: LT
              , actual: compareLength 0 [1] }
  assertEqual { expected: LT
              , actual: compareLength (-1) [] }

  assertTrue  $ isEmpty []
  assertFalse $ isEmpty [1]
  assertFalse $ isEmpty [1,2]
  assertFalse $ isEmpty [1,2,3]


  assertFalse $ isSingleton []
  assertTrue  $ isSingleton [1]
  assertFalse $ isSingleton [1,2]
  assertFalse $ isSingleton [1,2,3]

  let arrs = [ []
             , [1]
             , [1,2]
             , [1,2,3]
             , [1,2,3,4]
             , [1,2,3,4,5]
             ]
  for_ arrs \t1 -> do

    assertEqual { expected: length t1 == 1
                , actual: isSingleton t1 }

    assertEqual { expected: length t1 == 0
                , actual: isEmpty t1 }

    assertEqual { expected: length t1 :: Int
                , actual: unconsableLength t1 }

    assertTrue $ checkUnconsableLaws t1
    assertTrue $ checkUnconsableLaws (L.fromFoldable t1)
    assertTrue $ checkUnconsableLaws (LL.fromFoldable t1)
    assertTrue $ checkUnconsableLaws (CQ.fromFoldable t1)
    assertTrue $ checkUnconsableLaws (CL.fromFoldable t1)

    for_ [-2, -1, 0,1,2,3,4,5,6,7,8] $ \n -> do
      assertEqual { expected: length t1 > n
                  , actual: t1 `longerThan` n }

      assertEqual { expected: length t1 < n
                  , actual: t1 `shorterThan` n }

      assertEqual { expected: length t1 == n
                  , actual: t1 `hasLength` n }

    for_ arrs \t2 -> do
      assertEqual { expected: compare ((length t1) :: Int) (length t2)
                  , actual: t1 `compareLengths` t2 }

      assertEqual { expected: (length t1 :: Int) < length t2
                  , actual: (t1 `shorterThan` length t2 :: Int) }

      assertEqual { expected: length t1 > length t2 :: Int
                  , actual: t1 `isLongerThan` t2 }

      assertEqual { expected: length t1 < length t2 :: Int
                  , actual: t1 `isShorterThan` t2 }

      assertEqual { expected: length t1 == length t2 :: Int
                  , actual: t1 `isOfSameLength` t2 }

      -- log $ "longestTail " <> show t1 <> " " <>  show t2 <> " == " <> show (longestTail t1 t2)
      assertEqual { expected: longestTailOracle t1 t2
                  , actual: longestTail t1 t2 }
      assertEqual { expected: map (case _ of
                                     Left x -> x
                                     Right x -> x)
                                  (longestTailOracle t1 t2)
                  , actual: longestTail' t1 t2 }


longestTailOracle :: forall a b.
              Array a -> Array b -> Maybe (Either (Array a) (Array b))
longestTailOracle t1 t2 =
  let l1 = length t1 :: Int
      l2 = length t2 in
  case compare l1 l2 of
    GT -> Just $ Left  $ drop l2 $ t1
    LT -> Just $ Right $ drop l1 $ t2
    EQ -> Nothing
