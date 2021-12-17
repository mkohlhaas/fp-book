module Ch7a where

-- import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (<>))
-- import Data.Ord (class Ord, Ordering(..), compare)

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (<), (<=), (==), (>), (>=))

data Maybe a = Nothing | Just a

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--   eq Nothing   Nothing   = true
--   eq (Just a1) (Just a2) = a1 == a2
--   eq _         _         = false
  
-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing   Nothing   = EQ
--   compare Nothing   (Just _)  = LT
--   compare (Just _)  Nothing   = GT
--   compare (Just a1) (Just a2) = compare a1 a2

-- greaterThanOrEq :: ∀ a. Ord a => Maybe a -> Maybe a -> Boolean
-- greaterThanOrEq a1 a2 | compare a1 a2 == GT = true
-- greaterThanOrEq a1 a2 | compare a1 a2 == EQ = true
-- greaterThanOrEq _  _                        = false

-- infixl 4 greaterThanOrEq as >=

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing  = "Nothing"
--   show (Just a) = "(Just " <> show a <> ")"

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)
derive instance genericMaybeA :: Generic (Maybe a) _
instance showMaybeA :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

type MyEitherVar = Either String (Maybe Int)

test :: Effect Unit
test = do
  log $ show $ Just 5  == Just 5                    -- true
  log $ show $ Just 5  == Just 2                    -- false
  log $ show $ Just 5  == Nothing                   -- false
  log $ show $ Nothing == Just 5                    -- false
  log $ show $ Nothing == (Nothing :: Maybe Unit)   -- true
  log "------------------"
  log $ show $ Just 1  <  Just 5                    -- true
  log $ show $ Just 5  <= Just 5                    -- true
  log $ show $ Just 5  >  Just 10                   -- false
  log $ show $ Just 10 >= Just 10                   -- true
  log $ show $ Just 99 >  Nothing                   -- true
  log $ show $ Just 99 <  Nothing                   -- false
  log "------------------"
  log $ show $ Just "abc"                           -- (Just "abc")
  log $ show $ (Nothing :: Maybe Unit)              -- Nothing
  log "------------------"
  log $ show $ (Left "left" :: Either _ Unit)       -- (Left "left")
  log $ show $ (Right (Just 42) :: Either Unit _)   -- (Right (Just 42))
  let x = Left "left" :: MyEitherVar
      y :: MyEitherVar
      y = Right $ Just 42
  log $ show x                                      -- (Left "left")
  log $ show y                                      -- (Right (Just 42))