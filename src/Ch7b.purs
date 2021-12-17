module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Data.Int (fromString)

newtype CSV = CSV String
derive instance newtypeCSV :: Newtype CSV _ -- wrap & unwrap
derive newtype instance showCSV :: Show CSV
derive newtype instance eqCSV :: Eq CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String
derive instance newtypeFullName :: Newtype FullName _ -- wrap & unwrap
-- derive newtype instance showFullName :: Show FullName
instance showFullName :: Show FullName where
  show (FullName name) = name
derive newtype instance eqFullName :: Eq FullName

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _ -- wrap & unwrap
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow
derive instance eqOccupation :: Eq Occupation

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

instance showPerson :: Show Person where
  show (Person p) = show p.name <> "," <> show p.age <> "," <> show p.occupation
derive instance eqPerson :: Eq Person

-- derive newtype instance showAge :: Show Age
-- instance showFullName :: Show FullName where
--   show = unwrap

-- instance showAge :: Show Age where
--   show = unwrap >>> show

-- instance showOccupation :: Show Occupation where
--   show Doctor     = "Doctor"
--   show Dentist    = "Dentist"
--   show Lawyer     = "Lawyer"
--   show Unemployed = "Unemployed"

instance toCSVPerson :: ToCSV Person where
  toCSV :: Person -> CSV
  toCSV p = CSV $ show p

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [name, age, occupation] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person
         { name: FullName name
         , age: Age age'
         , occupation: occupation'
         }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing

toOccupation :: String -> Maybe Occupation
toOccupation "Doctor"     = Just Doctor
toOccupation "Dentist"    = Just Dentist
toOccupation "Lawyer"     = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _            = Nothing

test :: Effect Unit
test = do
  let person = Person { name: FullName "Sue Smith"
                      , age: Age 23
                      , occupation: Doctor
                      }
  log $ show $ (toCSV person # fromCSV) == Just person -- true



  -- log $ show $ toCSV
  --   (Person
  --     { name: FullName "Sue Smith"
  --     , age: Age 23
  --     , occupation: Doctor
  --     }) == CSV "Sue Smith,23,Doctor"