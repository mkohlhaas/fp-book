module Ch13 where

import Prelude (Unit, discard, ($), show, (/), class Show, (==), (<>), class Eq, identity, (+), (*), (<<<))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

---------------------- Maybe -----------------------------------
data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance genericMaybe:: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing  = Nothing
  map f (Just a) = Just $ f a

---------------------- Either -----------------------------------
data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left  a) = Left a
  map f (Right b) = Right $ f b

---------------------- Tuple ------------------------------------
data Tuple a b = Tuple a b

derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)
derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple a b) = Tuple a $ f b

---------------------- Threeple ---------------------------------
data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple a b c) = Threeple a b $ f c

---------------------- Bifunctor --------------------------------
class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

-- only map over left, leave right untouched
lmap :: ∀ f a b c. Bifunctor f => (a -> b) -> f a c -> f b c
lmap f = bimap f identity
-- lmap = flip bimap identity

-- only map over right, leave left untouched
rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

---------------------- Bifunctor Either -------------------------
instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left  a) = Left  $ f a
  bimap _ g (Right b) = Right $ g b

---------------------- Bifunctor Tuple --------------------------
instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple a b) = Tuple (f a) (g b)

---------------------- Bifunctor Threeple -----------------------
instance bifunctorThreeple :: Bifunctor (Threeple a) where
  bimap f g (Threeple a b c) = Threeple a (f b) (g c)

-----------------------------------------------------------------
test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ (_ / 2) <$> Threeple 10 20 40
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just [1, 2]) == Just [1, 2])
  log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just 1) == Just 1)
  log $ show $ "Maybe Composition for Nothing: " <> show ((map ((_*3) <<< (_+2)) Nothing) == (map (_*3) <<< map (_+2)) Nothing)
  log $ show $ "Maybe Composition for Just: " <> show ((map ((_*3) <<< (_+2)) (Just 60)) == (map (_*3) <<< map (_+2)) (Just 60))
  log $ show $ "Tuple Identity: " <> show ((identity <$> Tuple 10 20) == Tuple 10 20)
  log $ show $ "Tuple Composition : " <> show ((map ((_*3) <<< (_+2)) (Tuple 10 20)) == (map (_*3) <<< map (_+2)) (Tuple 10 20))
  log $ show $ rmap (_ * 2) $ Left "error reason"                    -- (Left "error reason")
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)            -- (Right 20)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit) -- (Left "ERROR REASON")
  log $ show $ lmap toUpper $ Right 10                               -- (Right 10)
  log $ show $ rmap  (_ * 2) $ Tuple 80 40                           -- Prints (Tuple 80 80)
  log $ show $ lmap  (_ / 2) $ Tuple 80 40                           -- Prints (Tuple 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40                   -- Prints (Tuple 40 80)
  log $ show $ rmap  (_ * 2) $ Threeple 99 80 40                     -- (Threeple 99 80 80)
  log $ show $ lmap  (_ / 2) $ Threeple 99 80 40                     -- (Threeple 99 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40             -- (Threeple 99 40 80)
