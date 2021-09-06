module Parser where

import Control.Applicative
--import Data.BigInt
import Data.Int
import Data.Char
import Data.EuclideanRing
import Prelude hiding (between)
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import Control.Alt ((<|>))
import Control.Monad (class Monad, pure)
import Data.Either (fromRight, Either)
import Data.Functor (($>))
import Data.List (List(..), many, (:), singleton, foldr, span, tail)
import Data.List.NonEmpty (cons', NonEmptyList) as NEL
import Data.List.Types (toList)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (length)

foreign import data BigInt :: Type

many1 :: forall m s a . Monad m => ParserT s m a -> ParserT s m (NEL.NonEmptyList a)
many1 p = NEL.cons' <$> p <*> many p

parens :: forall s a . StringLike s =>  Parser s a -> Parser s a
parens = between (string "(") (string ")") 

-- inside :: forall s . StringLike s => Parser s (NEL.NonEmptyList Char)
-- inside = parens (many1 anyChar)

-- test :: String -> Either ParseError String
-- test xs = runParser inside xs

digitInt :: Parser String Int
digitInt = map (\x -> readInt $ toList x) $ many1 digit

-- digitBigInt :: Parser String BigInt
-- digitBigInt = map (\x -> readBigInt $ toList x) $ many1 digit

-- digitの戻り値を変換
digit_ :: Parser String Number
digit_ = 
    let digitInt_ n k = if div n 10 == 0 then k else digitInt_ (div n 10) (k+1)
        digitInt n = digitInt_ n 1
        p1 = do
            x1 <- map (\x -> readInt $ toList x) $ many1 digit
            _  <- char '.'
            x2 <- map (\x -> readInt $ toList x) $ many1 digit
            let k = toNumber $ digitInt x2
            pure $ toNumber x1 + (toNumber x2 / k)
    in (map (\x -> readNum $ toList x) $ many1 digit) <|> p1 <?> "digit"

-- digitMinus = do
--     x <- string "-"
--     y <- digit_
--     pure $ -y

digit' = digit_ -- <|> digitMinus 

exprInt :: Parser String Int
exprInt = buildExprParser [
      [ Prefix (string "-" $> negate) ]
    , [ Infix (string "÷" $> div) AssocRight]
    , [ Infix (string "×" $> mul) AssocRight]
    , [ Infix (string "-" $> sub) AssocRight]
    , [ Infix (string "+" $> add) AssocRight]
    ] digitInt <?> "expression"

exprNumber :: Parser String Number
exprNumber = buildExprParser [
      [ Prefix (string"-" $> negate)]
    , [ Infix (string "÷" $> div) AssocRight]
    , [ Infix (string "×" $> mul) AssocRight]
    , [ Infix (string "-" $> sub) AssocRight]
    , [ Infix (string "+" $> add) AssocRight]
    ] digit' <?> "expression"


--runExprs :: Num a => String -> Parser String a -> Either ParseError a
runExprs xs expr = runParser xs expr

runExprInt xs = runExprs xs exprInt

runExprNumber xs = runExprs xs exprNumber


-- test :: Either ParseError Number
-- test = runParser "1*2+3" expr


test2 = sepBy (many digit) (string "+")

expr2 xs = fromRight (singleton $ singleton '0') $ runParser xs test2

readChar :: Char -> Int
readChar c = let c' = toCharCode c in  c' - 48

readInt_ :: List Char -> List Int -> Int
readInt_ Nil ys = foldr (+) 0 ys
readInt_ (Cons x xs) ys 
    | x == '-'  = readInt_ xs (f (-10) ys)
                    where f n ps = map (\x -> n*x) ps
    | otherwise = let x' = readChar x in readInt_ xs (Cons x' $ f 10 ys)
                    where f n ps = map (\x -> n*x) ps

readInt xs = readInt_ xs Nil

readNum :: List Char -> Number
readNum cs = 
    let p = span (\x -> not (x == '.')) cs
        xs = p.init
        ys = fromMaybe Nil $ tail p.rest
        n1 = toNumber $ readInt xs
        n2 = toNumber $ readInt ys
        k  = toNumber $ length $ show n2
    in n1 + (n2 / k)

summation cs = let ns = map readNum cs in foldr (+) 0.0 ns