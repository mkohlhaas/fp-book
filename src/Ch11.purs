module Ch11 where

import Prelude (Unit, show, type (~>), ($), flip, discard, negate, otherwise)
import Data.List (List(..), (:))
import Data.Foldable (class Foldable, foldl)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord (class Ord, (>=))
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

-- foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  -- log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil)
  -- log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
