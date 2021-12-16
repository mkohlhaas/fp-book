module Ch5 where
  
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (+), (-), (>), (<), (<=), (>=), (/=), (==), show, discard, negate, type (~>), max)

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

snoc :: ∀ a. List a -> a -> List a
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

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l   = Just $ init' l where
  init' :: List a -> List a
  init' Nil  = Nil
  init' (_ : Nil)  = Nil
  init' (a : as) = a : init' as

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (a : as) = Just $ {head: a, tail: as}

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (a : _) 0 = Just a
index (_ : as) i = index as (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex p l = findIndex' l 0 where
  findIndex' Nil _ = Nothing
  findIndex' (a : _ ) i | p a = Just i
  findIndex' (_ : as) i = findIndex' as (i + 1)

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p l = findLastIndex' l 0 Nothing where
  findLastIndex' :: List a -> Int -> Maybe Int -> Maybe Int
  -- ci = current index, ri = resultant index (Maybe)
  findLastIndex' Nil      _  ri           = ri
  findLastIndex' (a : as) ci _  | p a     = findLastIndex' as (ci + 1) $ Just ci
  findLastIndex' (_ : as) ci ri           = findLastIndex' as (ci + 1) ri

-- reverse :: List ~> List
reverse :: ∀ a. List a -> List a
reverse l = reverse' l Nil where
  reverse' :: List a -> List a -> List a
  -- rl = resulant list
  reverse' Nil      rl = rl
  reverse' (a : as) rl = reverse' as (a : rl)

-- concat :: ∀ a. List (List a) -> List a
-- concat ls = concat' ls Nil where
--   -- ls = list of lists
--   concat' :: List (List a) -> List a -> List a
--   -- rl = resulant list
--   concat' Nil        rl = reverse rl
--   concat' (l1 : l1s) rl = concat' l1s $ concat'' l1 rl where
--     -- l1 = list, l1s = list of lists
--     concat'' Nil      rl1 = rl1
--     concat'' (a : as) rl1 = concat'' as (a : rl1)

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil            = Nil
filter p (a : as) | p a = a : filter p as
filter p (_ : as)       =     filter p as

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil            = Nil
catMaybes (Nothing : as) =     catMaybes as
catMaybes (Just a  : as) = a : catMaybes as

range :: Int -> Int -> List Int
range = range' Nil where
  range' :: List Int -> Int -> Int -> List Int
  range' rl start end  | start < end = range' (end : rl) start (end - 1)
  range' rl start end  | start > end = range' (end : rl) start (end + 1)
  range' rl _     end                = end : rl

take :: ∀ a. Int -> List a -> List a
take n = take' Nil (max 0 n) where
  take' :: List a -> Int -> List a -> List a
  take' rl _  Nil               = reverse rl
  take' rl n' (a : as) | n' > 0 = take' (a : rl) (n' - 1) as
  take' rl _  _                 = reverse rl

take'' :: ∀ a. Int -> List a -> List a
take'' _ Nil      = Nil
take'' 0 _        = Nil
take'' n (x : xs) = x : take'' (n - 1) xs

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
  log $ show $ tail (Nil :: List Unit) -- Nothing
  log $ show $ tail ("abc" : "123" : Nil) -- (Just ("123" : Nil))
  log $ show $ (last Nil :: Maybe Unit) -- Nothing
  log $ show $ last ("a" : "b" : "c" : Nil) -- (Just "c")
  log $ show $ init ("a" : "b" : "c" : Nil) -- (Just ("a" : "b" : Nil))
  log $ show $ init (Nil :: List Unit) -- Nothing
  log $ show $ init (1 : Nil) -- (Just Nil)
  log $ show $ init (1 : 2 : Nil) -- (Just (1 : Nil))
  log $ show $ init (1 : 2 : 3 : Nil) -- (Just (1 : 2 : Nil))
  log $ show $ uncons (1 : 2 : 3 : Nil) -- (Just { head: 1, tail: (2 : 3 : Nil) })
  log $ show $ index (1 : Nil) 4 -- Nothing
  log $ show $ index (1 : 2 : 3 : Nil) 1 -- (Just 2)
  log $ show $ index (Nil :: List Unit) 0 -- Nothing
  log $ show $ index (1 : 2 : 3 : Nil) (-99) -- Nothing
  log $ show $ (1 : 2 : 3 : Nil) !! 1 -- (Just 2)
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil) -- (Just 1)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil) -- Nothing
  log $ show $ findIndex (10 /= _) (Nil :: List Int) -- Nothing
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int) -- Nothing
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) -- (Just 5)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil) -- Nothing
  log $ show $ reverse (10 : 20 : 30 : Nil) -- (30 : 20 : 10 : Nil)
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil) --  (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil) -- (1 : 2 : 3 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil) -- (1 : 2 : 5 : Nil)
  log $ show $ range 1 10 -- (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  log $ show $ range 3 (-3) -- (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  log $ show $ take 5 (12 : 13 : 14 : Nil) -- (12 : 13 : 14 : Nil)
  log $ show $ take'' 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil) -- (-7 : 9 : 0 : 12 : -13 : Nil)
  log $ show $ take'' 5 (12 : 13 : 14 : Nil) -- (12 : 13 : 14 : Nil)
  log $ show $ take'' 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil) -- (-7 : 9 : 0 : 12 : -13 : Nil)
