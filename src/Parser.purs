module Parser where

import Prelude
import Data.Tuple (Tuple)
import Data.Either (Either)
import Effect (Effect)
import Effect.Console (log)

-- data ParserState a = ParserState String a
type ParserState a     = Tuple String a
type ParseFunction e a = ParseError e => String -> Either e (ParserState a)
newtype Parser e a     = Parser (ParseFunction e a)

class ParseError (e :: Type)

instance functorParser :: Functor (Parser e) where
  map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
  map f (Parser g) = Parser \s -> f <$> g s

test :: Effect Unit
test = do
  log "placeholder"
