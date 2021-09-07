module Parser(
              number,
              isNumber,
              exprInt,
              exprNumber,
              runParser',
              runExprInt,
              includePeriod,
              predicateByParser,
              isEndPeriod,
              isEndOperator,
              isOperators,
              runExprNumber,
              digitNumber,
              digitInt) where

import Control.Applicative
import Data.Char
import Data.EuclideanRing
import Data.Int
import Prelude hiding (between)
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import Control.Alternative ((<|>))
import Control.Monad (class Monad, pure)
import Data.Array (fromFoldable, replicate)
import Data.Either (fromRight, Either(..))
import Data.Functor (($>))
import Data.List (List(..), many, (:), singleton, foldr, span, tail)
import Data.List.NonEmpty (cons', NonEmptyList) as NEL
import Data.List.Types (toList)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray)

--import Data.Rational()

foreign import data BigInt :: Type

many1 :: forall m s a . Monad m => ParserT s m a -> ParserT s m (NEL.NonEmptyList a)
many1 p = NEL.cons' <$> p <*> many p

parens :: forall s a . StringLike s =>  Parser s a -> Parser s a
parens = between (string "(") (string ")") 


listToString :: NEL.NonEmptyList Char -> String
listToString l = fromCharArray $ fromFoldable l


intPartOfnumber :: Parser String Number
intPartOfnumber = map (\l -> fromMaybe 0.0 $ fromString $ listToString l) $ many1 digit

number :: Parser String Number
number = do
    n1 <- intPartOfnumber
    _  <- string "."
    n2 <- intPartOfnumber
    let k = length $ show n2
    pure $ n1 + (n2 / foldr (*) 1.0 (replicate k 10.0))

isNumber :: String -> Boolean
isNumber xs = 
    let p = do
            _ <- many1 (satisfy $ \x -> not $ x == '.')
            x <- char '.'
            _ <- many (satisfy $ \x -> not $ x == '.')
            pure x
        rslt = runParser xs p
    in case rslt of
        Left _  -> false
        Right _ -> true
        
includePeriod :: String -> Boolean
includePeriod xs = isNumber xs

isEndPeriod :: String -> Boolean
isEndPeriod xs = 
    let last zs = let n = length zs in drop (n-1) zs
    in last xs == "."

-- パーサーを与えると、Stringに関する術後を返す関数。
    -- マッチング成功でtrue,失敗でfalseを返す
    -- 「末尾の文字が何々」のような、文字の位置情報に関しては難しい。というのはマッチングが甘々なので。
predicateByParser :: forall s a . StringLike s => s -> Parser s a -> Boolean
predicateByParser xs ps = 
    let rslt = runParser xs ps
    in case rslt of
        Left _ -> false
        Right _ -> true

operators :: forall s . StringLike s => Parser s Char
operators = choice $ map char ['+','-','×','÷']

isOperators :: String -> Boolean
isOperators xs = predicateByParser xs operators

isEndOperator :: String -> Boolean
isEndOperator xs =
    let last zs = let n = length zs in drop (n-1) zs
    in isOperators xs


runParser' = runParser


digitInt :: Parser String Int
digitInt = map (\x -> readInt $ toList x) $ many1 digit


-- digitの戻り値を変換
digitNumber :: Parser String Number
digitNumber = 
    let intstr    = map toNumber digitInt-- Intとも取れる数はまずIntとして解析し、それをNumberに変換する
        numberstr = do
            xs <- many1 digit
            p  <- option "." $ string "."
            y  <-  many  digit
            let x' = listToString xs
                y' = fromCharArray $ fromFoldable y
            map (\z -> fromMaybe 0.0 $ fromString z) $ pure $ x' <> p <> y'
    in numberstr <?> "digit" -- オルタナは左を優先的に調べるので、先にIntがマッチングする不具合は防げるはず
    -- オルタナティブが仕事してくれてないのは何故なのか？


exprInt :: Parser String Int
exprInt = buildExprParser [
      [ Prefix (string "-" $> negate) ]
    , [ Infix (string "÷" $> div) AssocRight]
    , [ Infix (string "×" $> mul) AssocRight]
    , [ Infix (string "-" $> sub) AssocRight]
    , [ Infix (string "+" $> add) AssocRight]
    ] digitInt <?> "expression"

exprNumber :: Parser String Number
exprNumber  = buildExprParser [
      [ Prefix (string"-" $> negate)]
    , [ Infix (string "÷" $> div) AssocRight]
    , [ Infix (string "×" $> mul) AssocRight]
    , [ Infix (string "-" $> sub) AssocRight]
    , [ Infix (string "+" $> add) AssocRight]
    ] digitNumber <?> "expression"


--runExprs :: Num a => String -> Parser String a -> Either ParseError a
runExprs xs expr = runParser xs expr

runExprInt xs = runExprs xs exprInt

runExprNumber :: String -> Either ParseError Number
runExprNumber xs = runParser xs exprNumber 
                

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
    let xs = fromFoldable cs
        n  = fromMaybe 0.0 $ fromString $ fromCharArray xs
    in n

summation cs = let ns = map readNum cs in foldr (+) 0.0 ns

-- -- 文字列段階で式に干渉し、Int値と見做せる式をNumber値に解釈可能にするためのパーサー
-- exprString :: Parser String String
-- exprString  = buildExprParser [
--       [ Prefix (string"-" $> (\x -> "" <> x))]
--     , [ Infix (string "÷" $> (<>)) AssocRight]
--     , [ Infix (string "×" $> (<>)) AssocRight]
--     , [ Infix (string "-" $> (<>)) AssocRight]
--     , [ Infix (string "+" $> (<>)) AssocRight]
--     ] putPeriod <?> "expression"

-- putPeriod :: Parser String String
-- putPeriod = do
--   x <- digitNumber
--   Position pos <- position
--   let x' = show x
--   let n  = line pos
--   pure $ show x <> "."

