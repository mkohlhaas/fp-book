module Ch15 where

import Prelude (Unit, discard, mod, show, ($), (+), (<>), (<<<), (==), identity, class Semiring, zero)
import Data.Foldable (class Foldable, foldl)
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Profunctor
import Data.String.CodePoints
import Effect (Effect)
import Effect.Console (log)

data Predicate a = Predicate (a -> Boolean)
-- type Predicate a = (->) a Boolean

instance contravariantPredicate :: Contravariant Predicate where
  -- cmap :: (b -> a) -> f a -> f b
  cmap f (Predicate p) = Predicate (p <<< f)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

odd' :: Int -> Boolean
odd' x = x `mod` 2 == 1

---------------------------------------------------------------------------
-------------------- Moore Machine ----------------------------------------
---------------------------------------------------------------------------

data Moore s a b = Moore s (s -> b) (s -> a -> s)
-- contravariant in a, covariant in b => Profunctor

-- class Profunctor p where
--   dimap :: ∀ a b c d. (b -> a) -> (c -> d) -> p a c -> p b d       (Contravariant in a, Covariant in c)

-- Moore s f1 f2
-- p = Moore s
-- p a c = Moore s (s -> c) (s -> a -> s)
-- p b d = Moore s (s -> d) (s -> b -> s)
-- dimap :: (b -> a) -> (c -> d) -> (Moore s (s -> c) (s -> a -> s)) -> (Moore s (s -> d) (s -> b -> s))

----------------------
-- Partial Application
----------------------
-- fn x = 5 + x
-- fn = \x -> 5 + x
-- myAdd x y = x + y
-- myAdd   y = \x -> x + y
-- myAdd     = \x -> \y -> x + y
-- f2 s a = fn-body             :: s -> a -> s
-- f2   a = \s -> fn-body       :: s -> a -> s
-- f2     = \s -> \a -> fn-body :: s -> a -> s

instance profunctorMoore :: Profunctor (Moore s) where
  -- dimap f g (Moore s f1 f2) = Moore s (g <<< f1) (\s1 -> (f2 s1 <<< f))
  dimap f g (Moore s f1 f2) = Moore s (g <<< f1) (\s' -> f2 s' <<< f)

addr :: ∀ a. Semiring a => Moore a a a
--     Moore Int    (Int -> Int) (Int -> Int -> Int)
addr = Moore zero   identity     (+)

runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s a b) = a <<< foldl b s

sizer :: Moore Int String String
sizer = dimap length (\n -> "Size is " <> show n) addr

---------------------------------------------------------------------------
test :: Effect Unit
test = do
  log $ show $ odd  0
  log $ show $ odd  1
  log $ show $ odd' 0
  log $ show $ odd' 1
  log "------------------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$<  (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$<  (Predicate odd)) 10
  log "------------------------------------"
  log $ show $ runFoldL addr [1, 2, 3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]  -- "Size is 13".
