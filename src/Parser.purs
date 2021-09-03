module Parser where
  
-- import Text.Parsing.Parser
-- import Text.Parsing.Parser.Combinators
-- import Text.Parsing.Parser.String

-- import Control.Monad (class Monad)
-- import Data.Either (fromRight, Either)
-- import Prelude ((+), (-), (*))
-- import Prim(Char,String)

-- parens :: forall m s a open close. Monad m => StringLike s =>  ParserT s m a -> ParserT s m a
-- parens = between (string "(") (string ")") 

-- inside :: forall m s open close . Monad m => StringLike s => ParserT s m Char
-- inside = parens (anyChar)

-- test :: forall m open close . Monad m => Char -> m (Either ParseError Char)
-- test xs = runParser inside xs