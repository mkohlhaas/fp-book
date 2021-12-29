module Ch17 where

import Prelude
import Data.Bifunctor (class Bifunctor)
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)

-------------------------------------------------------------------
------------------------------ Maybe ------------------------------
-------------------------------------------------------------------

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing  = Nothing
  map f (Just a) = Just $ f a

-- instance applyMaybe :: Apply Maybe where
--   -- apply :: f (a -> b) -> f a -> f b
--   apply Nothing  _        = Nothing
--   apply _        Nothing  = Nothing
--   apply (Just f) (Just a) = Just $ f a

instance applyMaybe :: Apply Maybe where
  apply (Just f) a = f <$> a
  apply Nothing  _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

-------------------------------------------------------------------
------------------------------ Either -----------------------------
-------------------------------------------------------------------

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance functorEither :: Functor (Either a)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left  a) = Left  $ f a
  bimap _ g (Right b) = Right $ g b

instance applyEither :: Apply (Either e) where
  apply (Left  e) _ = Left e
  apply (Right f) r = f <$> r

instance applicativeEither :: Applicative (Either a) where
  pure = Right


-------------------------------------------------------------------
------------------------------ Validation -------------------------
-------------------------------------------------------------------

newtype Validation err result = Validation (Either err result)

derive instance newtypeValidation :: Newtype (Validation err result) _
derive newtype instance functorValidation :: Functor (Validation err)
derive newtype instance bifunctorValidation :: Bifunctor Validation
derive newtype instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)
derive newtype instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)
derive instance genericValidation :: Generic (Validation err result) _
instance showValidation :: (Show err, Show result) => Show (Validation err result) where
  show = genericShow

instance applyValidation :: Semigroup err => Apply (Validation err) where
  -- apply :: f (a -> b) -> f a -> f b
  apply (Validation (Left err1)) (Validation (Left err2))    = (Validation (Left $ err1 <> err2))
  apply (Validation (Left err))  _                           = (Validation (Left err))
  apply (Validation (Right f))   result                      = f <$> result

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure = Validation <<< Right

-------------------------------------------------------------------
test :: Effect Unit
test = do
  log $ show $ (+) <$> Just 21 <*> Just 21               -- (Just 42)
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int) -- (Just 42)
  log $ show $ pure (+) <*> Just 17 <*> Just 25          -- (Just 42)
  -- LAW: Associative Composition
  -- (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- LAW: Identity
  -- pure identity <*> x = x
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- LAW: Homomorphism
  -- pure (f x) = pure f <*> pure x
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- LAW: Interchange
  -- u <*> pure x = pure (_ $ x) <*> u
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
