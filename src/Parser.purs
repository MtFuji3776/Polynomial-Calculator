module Parser where

import Data.Char
import Data.EuclideanRing
import Prelude hiding (between)
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import Control.Monad (class Monad)
import Data.Either (fromRight, Either)
import Data.Functor (($>))
import Data.List (List(..), many, (:), singleton, foldr)
import Prelude ((+), (-), (*), div)

-- import Control.Applicative


-- many1 :: forall m s a . Monad m => ParserT s m a -> ParserT s m (NEL.NonEmptyList a)
-- many1 p = NEL.cons' <$> p <*> many p

-- parens :: forall s a . StringLike s =>  Parser s a -> Parser s a
-- parens = between (string "(") (string ")") 

-- inside :: forall s . StringLike s => Parser s (NEL.NonEmptyList Char)
-- inside = parens (many1 anyChar)

-- test :: String -> Either ParseError String
-- test xs = runParser inside xs

-- expr :: Parser String Char
-- expr = buildExprParser [
--       [ Infix (string "/" $> (div)) AssocRight]
--     , [ Infix (string "*" $> mul) AssocRight]
--     , [ Infix (string "-" $> sub) AssocRight]
--     , [ Infix (string "+" $> add) AssocRight]
--     ] digit


-- test :: Either ParseError Char
-- test = runParser "1*2+3" expr


test1 = string "test"

expr1 = runParser "testです" test1

test2 = sepBy (many digit) (string "+")

expr2 xs = fromRight (singleton $ singleton '0') $ runParser xs test2

readChar :: Char -> Int
readChar c = let c' = toCharCode c in c' - 48

readNum_ :: List Char -> List Int -> Int
readNum_ Nil ys = foldr (+) 0 ys
readNum_ (Cons x xs) ys = 
    let x' = readChar x
    in readNum_ xs (Cons x'  (map (\y -> 10*y) ys))

readNum xs = readNum_ xs Nil

summation cs = let ns = map readNum cs in foldr (+) 0 ns