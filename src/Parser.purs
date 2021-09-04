module Parser where

import Prelude hiding(between)

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Control.Monad (class Monad)
import Data.Either (fromRight, Either)
import Prelude ((+), (-), (*))
import Prim(Char,String)
import Data.List.NonEmpty as NEL
import Data.List (List(..),many,(:))
import Control.Applicative

-- many1 :: forall m s a . Monad m => ParserT s m a -> ParserT s m (NEL.NonEmptyList a)
-- many1 p = NEL.cons' <$> p <*> many p

-- parens :: forall s a . StringLike s =>  Parser s a -> Parser s a
-- parens = between (string "(") (string ")") 

-- inside :: forall s . StringLike s => Parser s (NEL.NonEmptyList Char)
-- inside = parens (many1 anyChar)

-- test :: String -> Either ParseError String
-- test xs = runParser inside xs