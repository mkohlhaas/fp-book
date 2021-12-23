module Ch11 where

import Prelude (Unit, show, type (~>), ($), flip, discard, negate, otherwise, (+), (<>), (<<<))
import Data.List (List(..), (:), singleton, foldl, foldr, foldMap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Foldable (class Foldable)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, (>=))
import Data.Semiring
import Effect (Effect)
import Effect.Console (log)

reverse :: List ~> List
reverse = foldl (flip (:)) Nil

max :: ∀ a. Ord a => a -> a -> a
max a1 a2 | a1 >= a2  = a1
          | otherwise = a2

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil      = Nothing
findMax (a : as) = Just $ foldl max a as

findMaxNE :: ∀ a. Ord a => NonEmptyList a  -> a
findMaxNE (NonEmptyList (a :| as)) = foldl max a as

findMaxNE1 :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE1 (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
-- foldl1 fun (NonEmpty a fa) = foldl fun a fa
foldl1 fun (a :| fa) = foldl fun a fa

sum :: List Int -> Int
sum Nil      = 0
sum (i : is) = i + sum is

sum1 :: List Int -> Int
sum1 = go 0 where
  go acc Nil      = acc
  go acc (i : is) = go (i + acc) is

sum2 :: List Int -> Int
sum2 = foldl (+) 0

sum3 :: ∀ a f. Semiring a => Foldable f => f a -> a
sum3 = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf leaf)       = singleton leaf
toList (Node left right) = toList left <> toList right

instance foldableTree :: Foldable Tree where
  foldl fn acc = foldl fn acc <<< toList
  foldr fn acc = foldr fn acc <<< toList
  foldMap fn   = foldMap fn <<< toList

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  -- log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil)
  -- log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE  (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE1 (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE  (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ findMaxNE1 (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ sum  (1 : 2 : 3 : Nil)
  log $ show $ sum1 (1 : 2 : 3 : Nil)
  log $ show $ sum2 (1 : 2 : 3 : Nil)
  log $ show $ sum3 (1 : 2 : 3 : Nil)
  log $ show $ sum3 (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ sum3 [1, 2, 3]
  log $ show $ sum3 [1.0, 2.0, 3.0]
  log $ show $ toList (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) -- (5 : -1 : 14 : 99 : Nil)
  log $ show $ sum3   (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99)) -- 117
