module Calculus where
  
import Prelude(class Eq,class Ord,class Show, show , ($),(+),(*),(<>),(==))
import Data.Functor
import Data.Map as M
import Data.Map (Map,toUnfoldable)
import Data.Tuple
import Data.Maybe
-- import Control.Fold(mconcat)
import Data.Foldable(foldr)

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

data Unit = Unit Variables Int

un :: Unit
un = Unit (M.fromFoldable [Tuple 3 (VD 3 4), Tuple 7 (VD 7 5)]) 9

derive instance eqUnit  :: Eq Unit
derive instance ordUnit :: Ord Unit

instance showUnit :: Show Unit where
    show (Unit vs c) = 
        let vs' = foldr (<>) "" $ map show $ vs
        in if vs' == "" then show c else show c <> "*" <> vs'

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