module Ch5 where
  
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (+), show, discard)
import Data.Maybe (Maybe(..))

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip fn a b = fn b a

const :: ∀ a b. a -> b -> a
const a _ = a

apply :: ∀ a b. (a -> b) -> a -> b
apply fn = fn

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton a = a : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _   = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x
snoc (x : xs) y = x : snoc xs y

-- length :: ∀ a. List a -> Int
-- length Nil = 0
-- length (_ : xs) = 1 + length xs

length :: ∀ a. List a -> Int
length = length' 0 where
  length' :: Int -> List a -> Int
  length' acc Nil = acc
  length' acc (_ : xs) = length' (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

test :: Effect Unit
test = do
  log $ show $ flip const 1 2 -- 2
  flip const 1 2 # show # log -- 2
  log $ show $ singleton "xyz" -- ("xyz" : Nil)
  singleton "xyz" # show # log -- ("xyz" : Nil)
  log $ show $ null Nil -- true
  log $ show $ null ("abc" : Nil) -- false
  log $ show $ snoc (1 : 2 : Nil) 3 -- (1 : 2 : 3 : Nil)
  log $ show $ length $ 1 : 2 : 3 : Nil -- 3
  log $ show (head Nil :: Maybe Unit) -- Nothing
  log $ show $ head (Nil :: List Unit) -- Nothing
  log $ show $ head ("abc" : "123" : Nil) -- (Just "abc")

