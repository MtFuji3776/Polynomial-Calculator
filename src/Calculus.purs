module Calculus where
  
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Ring
import Data.Semiring
import Data.Tuple

import Data.Array ((:), head, zipWith, tail)
import Data.Foldable (foldr)
import Data.Map (Map, toUnfoldable)
import Data.Map as M
import Prelude (class Eq, class Ord, class Show, class Functor, show, ($), (+), (*), (<>), (==), otherwise)


data VarData = VD Int Int

vd :: VarData
vd = VD 3 4

derive instance eqVarData  :: Eq VarData
derive instance ordVarData :: Ord VarData

instance showVarData :: Show VarData where
    show (VD n m) = "[X" <> show (n) <> " ^ " <> show (m) <> "]"

type Variables = Map Int VarData

varProd :: VarData -> VarData -> Variables
varProd (VD n1 m1) (VD n2 m2) = if  VD n1 m1 ==   VD n2 m2 
            then M.singleton n1 $ VD n1 (m1 + m2)
            else M.fromFoldable [Tuple n1 (VD n1 m1),Tuple n2 (VD n2 m2)]

multVarData :: VarData -> VarData -> Maybe VarData
multVarData (VD v1 d1) (VD v2 d2) = 
    if v1 == v2
        then Just $ VD v1 (d1 + d2)
        else Nothing

-- Intの部分を抽象化したいが、Semiring型クラスのインスタンスに制限しないとShowの定義などで引っかかる。
    -- ここはどうすれば良いか？
data Unit = Unit Variables Int

un :: Unit
un = Unit (M.fromFoldable [Tuple 3 (VD 3 4), Tuple 7 (VD 7 5)]) 9

derive instance eqUnit  :: Eq Unit
derive instance ordUnit :: Ord Unit


instance showUnit :: Show Unit where
    show (Unit vs c) = 
        let vs' = foldr (<>) "" $ map show $ vs
        in if c == 0
            then ""
            else
                if vs' == "" 
                 then show c 
                 else show c <> "*" <> vs'

addCoefficients :: Unit -> Unit -> Maybe Unit
addCoefficients (Unit vd1 c1) (Unit vd2 c2) =
    if vd1 == vd2
        then Just $ Unit vd1 (c1 + c2)
        else Nothing

multipleUnits :: Unit -> Unit -> Unit
multipleUnits (Unit vd1 c1) (Unit vd2 c2) =
    let vd = M.unionWith (\x y -> fromMaybe (VD 0 0) $ multVarData x y) vd1 vd2
        c  = c1 * c2
    in Unit vd c

newtype Poly = Poly (Map Variables Unit)

derive instance eqPoly  :: Eq Poly
derive instance ordPoly :: Ord Poly


poly :: Poly
poly = 
    let f xs ys = M.fromFoldable $ zipWith Tuple xs $ zipWith VD xs ys
        vs1     = f [0,1] [2,3]
        vs2     = f [2] [1]
        u1  = Unit vs1 1
        u2  = Unit vs2 2
    in Poly $ M.fromFoldable $ zipWith Tuple [vs1,vs2] [u1,u2]

-- 定数項nを生成する関数
constantPoly :: Int -> Poly
constantPoly n = Poly $ M.singleton M.empty (Unit M.empty n)

instance showPoly :: Show Poly where
    show (Poly m) =
        let l = map show m
            drawblock x y 
                | x == "" = y
                | y == "" = x
                | otherwise = x <> " + " <> y
            xs = foldr drawblock "" l
        in xs

-- purescriptのData.MapにはinsertWithが存在しないので自作
insertWith :: forall v k . Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v m
    | M.member k m = 
            let x  = fromMaybe v $ M.lookup k $ m
                x' = f v x
            in M.insert k x' m
    | otherwise    = M.insert k v m

multiplePoly :: Poly -> Poly -> Poly
multiplePoly (Poly m1) (Poly m2) = 
    let f :: M.Map Variables Unit -> Array Unit
        f m = map snd $ toUnfoldable m
        l1 = f m1
        l2 = f m2
        l  = map (\(Unit vs c) -> Tuple vs (Unit vs c)) $ multipleUnits <$> l1 <*> l2
        reduction :: Array (Tuple Variables Unit) -> Poly -> Poly
        reduction [] (Poly m3)     = Poly m3
        reduction xs (Poly m3) = 
            let x  = fromMaybe (Tuple M.empty (Unit M.empty 0)) $ head xs
                vs = fst x
                u  = snd x
                addKeisuu v w = fromMaybe (Unit M.empty 0) $ addCoefficients v w
                p' = Poly $ insertWith addKeisuu vs u m3
            in reduction (fromMaybe [] $ tail xs) p'
    in reduction l (Poly M.empty)

instance semiringPoly :: Semiring Poly where
    add (Poly m1) (Poly m2) = 
            let f x y = fromMaybe (Unit M.empty 0) $ addCoefficients x y 
            in Poly $ M.unionWith f m1 m2
    zero = constantPoly 0
    mul  = multiplePoly
    one  = constantPoly 1

-- instance ringPoly :: Ring Poly where
--     sub p1 (Poly m2) = 
--         let negate (Unit v c) = Unit v (-c)
--             p2' = Poly $ map negate m2
--         in add p1 p2'