module Main where

import Data.Boolean
import Halogen.HTML.CSS
import Parser
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Config (expo) as Config
import Data.Array (foldr, zipWith)
import Data.Either (Either(..), fromRight)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Number (fromString)
import Data.String (singleton, uncons, length, take, drop, contains, dropWhile, null, Pattern(..))
import Data.String.CodePoints (codePointFromChar)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Random (random)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff) as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements as HHE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import StyleSheet (expo)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Text.Parsing.Parser(runParser)

--import Web.HTML.HTMLDocument (body)




main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data ActNum = Zero
            | One
            | Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
  
instance showActNum :: Show ActNum where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"

data ActOperator = Plus | Minus | Times | Divide

instance showActOperator :: Show ActOperator where
  show Plus   = "+"
  show Minus  = "-"
  show Times  = "×"
  show Divide = "÷"
  

data ActSpecial = Equal | C | AC | Period | Substitute

instance showActSpecial :: Show ActSpecial where
  show Equal      = "="
  show C          = "C"
  show AC         = "AC"
  show Period     = "."
  show Substitute = "sub"

data Action = AN ActNum | AO ActOperator | AS ActSpecial


data PositionOfPeriod = NoExists | MiddleOfNI | EndOfNI

derive instance eqPositionOfPEriod :: Eq PositionOfPeriod 

type State = { formula     :: String
             , actOperator :: Maybe ActOperator
             , newInputs   :: String
             , positionOfPeriod :: PositionOfPeriod
             , inputLog    :: Array {formula_::String, newInputs_ :: String}
             , counter     :: Int
             , evalLog     :: Array Int
             }

component :: forall query input output m . MonadAff m => H.Component query input output m
component =
  H.mkComponent
    {
      initialState
    , render
    , eval : H.mkEval $ H.defaultEval {handleAction = handleAction}
    } -- レコード型の一種だろうか？これを引数にとってcomponentが生成される。
  
initialState :: forall input . input -> State
initialState _ = { formula:   ""
                 , actOperator: Nothing
                 , newInputs: "0"
                 , positionOfPeriod: NoExists
                 , inputLog:  []
                 , counter:    0
                 , evalLog:   []
                 }

initialState_ = initialState 0

render :: forall m . State -> H.ComponentHTML Action () m
render st =
  HHE.body [HP.id_ "index"] 
    [
      HHE.head 
        []
        [
          HHE.meta [HP.charset "UTF-8",HP.name "viewpoint"]
        , HHE.title [] [HH.text "Web電卓"]
        , HHE.link [HP.href "https://fonts.googleapis.com/css?family=Sawarabi+Mincho|Ultra&display=swap"]
        ]
      ,
      let classname nm = HP.class_ $ ClassName nm
      in 
      HHE.body
        [HP.id "index"]
          [HHE.header_ [HHE.h1_ [HH.text "WEB電卓"]]
        , HH.main_
            [
              HHE.div [HP.class_ $ ClassName "main-content" ]
                [
                  let divBtn xs act = HHE.div [HP.class_ $ ClassName "btn",HE.onClick \_ -> act] [HH.text xs]
                  in
                  HHE.div [HP.class_ $ ClassName "content-area"]
                  [
                    HHE.div [HP.class_ $ ClassName "display-area"]
                      [
                        HHE.div [HP.class_ $ ClassName "num_display"]
                        [
                          HH.text $ let fm = st.formula
                                        ni = st.newInputs
                                      in if fm == "" && ni == "0" 
                                        then ni
                                        else if ni == "+" || ni == "-" || ni == "×" || ni == "÷"
                                          then fm <> ni
                                          else fm <> ni
                        ]
                      , HHE.div [HP.class_$ ClassName "howcal_display"] []
                      ]
                  , HHE.div [HP.class_ $ ClassName "click-area"]
                    [
                        HHE.div [HP.class_ $ ClassName "operator"]
                        [
                          divBtn "+" $ AO Plus
                        , divBtn "-" $ AO Minus
                        , divBtn "×" $ AO Times
                        , divBtn "÷" $ AO Divide
                        , divBtn "AC" $ AS AC
                        ]
                      , HHE.div [classname "operator"]
                        [
                          HHE.div [classname "row1"]
                            [
                              divBtn "7" $ AN Seven
                            , divBtn "8" $ AN Eight
                            , divBtn "9" $ AN Nine
                            ]
                        , HHE.div [classname "row2"]
                            [
                              divBtn "4" $ AN Four
                            , divBtn "5" $ AN Five
                            , divBtn "6" $ AN Six
                            ]
                        , HHE.div [classname "row3"]
                            [
                              divBtn "1" $ AN One
                            , divBtn "2" $ AN Two
                            , divBtn "3" $ AN Three
                            ]
                        , HHE.div [classname "row4"]
                            [
                              divBtn "0" $ AN Zero
                            , divBtn "." $ AS Period
                            , divBtn "C" $ AS C
                            ]
                        , HHE.div [classname "row5"]
                            [
                              divBtn "=" $ AS Equal
                            ]
                        ]
                    ]
                  ]

                ]
            ]
          , HHE.footer_ 
              [
                HHE.small_ [HH.text "(c) 2019 momoogles."]
              ]


        , stylesheet $ do
                        expo
                        Config.expo
        ]
        
    ]
-- 電卓のとりうる状況からオートマトンを設計し、それをhandleActionに翻訳する
  -- これはStateの状態がq0であることを判定する述語。
q0 x y = x == "" && y == "0" && not (includePeriod $ x<>y)
  -- 状態がq1であることを判定。
q1 x y = x == "" && (not (y == "0" || y == "")) && not (includePeriod $ x <> y)
  -- q2は、式が評価可能であるときに＝を押した場合に遷移する状態。
q2 x y = (not (x == "")) && y == "" && not (includePeriod $ x <> y)
-- q3は、formula,newInputsともに空列ではない状態。演算記号を入力し、さらに第二引数を入力している途中の状態である。
q3 x y = not (x == "" || y == "") && not (includePeriod $ x <> y)
-- q4は
q4 x y = (not (x == "")) && (y == "+" || y == "-" || y == "×" || y == "÷") && not (includePeriod $ x <> y)

q5 x y = (not (x == "")) && (y == "0") && not (includePeriod $ x <> y)

last :: String -> String
last xs = let n = length xs in drop (n-1) xs
-- 以下は入力列の中にピリオドが含まれる場合の状態たち。
q1' x y = x == "" && y == "0." -- 初期状態の直後にピリオドが入力された状態

-- 開始後、演算記号を打ち込むより先にピリオドが打たれた状態
q2' x y = x == "" && includePeriod y

-- 一回以上演算記号が押され、かつ現在入力中の数の末尾がピリオドの状態
  -- 次のq4'の部分状態だが、この状況でピリオドやイコールを押した場合の挙動を定義しなければならないので用意する意味がある。
q3' x y = not (x == "") && isEndPeriod y

-- 一回以上演算記号が押され、かつ現在入力中の数は小数である状態
q4' x y = not (null x) && includePeriod y

-- 演算記号または等号を押したため、状態がまとまってて現在入力中の数が存在しない状態。
q5' x y = includePeriod x && null y

-- 最後に演算記号を打ったよりも前にピリオドを打っており、さらに今打っている数もピリオドを含んでいる状態。
q6' x y = includePeriod x && includePeriod y -- 条件として一番ゆるい述語。q7',q9',q4'など他の状態を含んでいるので、これはガードの一番最後に回すべき。

-- 現在入力中の数の末尾はピリオドである。q3'のサブ状態だが、これを用意する意図はなんだろうか？q3'で代用できるのでは？
q7' x y = includePeriod x && isEndPeriod y

-- q3'の対になる主張。これはq6'、q7'より更に守備範囲が広い。ガードの末尾へ。
q8' x y = includePeriod x && not (null y)

-- 重要。今まで打った数が小数であり、かつ今の時点で演算記号を打ち込んでいる。
q9' x y = includePeriod x && isOperators y

-- 演算記号を押すまでは少数を打っていたが、今打っている数は整数である状態（パーサーの改良によりこれは不要かも？）
q10' x y = includePeriod x && not (includePeriod y)

-- 状態遷移コンビネータたち
  -- handleActionはこれらを組み合わせて定義する
  -- Stateモナドにしても良さそうものだが。
init :: String -> String
init xs = let n = length xs in take (n-1) xs


appendInput st x = let ni = st.newInputs in st{newInputs = ni <> show x}

appendFM st = st{formula = st.formula <> st.newInputs}

cutEndOfNI st = st{newInputs = init st.newInputs}

evalState st = st{formula = evalFormula st}

putInput st x = st{newInputs = show x}

resetFM st = st{formula = ""}

resetNI st = st{newInputs = ""}




evalFormula :: State -> String
evalFormula st =
    let zs = st.formula <> st.newInputs
        zs' = if includePeriod zs
                  then show $ fromRight 0.0 $ runParser zs exprNumber
                  else show $ fromRight 0   $ runParser zs exprInt
    in zs'
-- なんと、handleActionをよく見れば、Actionを引数に取る関数である。
  -- StateとActionをオートマトンと見做す時、handleActionは遷移関数に相当するが、その定義を入力文字別に場合分けして書き下すことが義務付けられているということ。
  -- 正直Stateベースで場合分けする方が書きやすい…
handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction  = case _ of 
        AN x -> H.modify_ \st -> 
          let fm = st.formula
              ni = st.newInputs
          in if not $ includePeriod $ fm <> ni -- fm,niのどちらにもピリオドが含まれない時。このとき数はIntとして計算される
              then 
                let nextState
                      | q0 fm ni = putInput st x
                      | q5 fm ni = putInput st x
                      | q1 fm ni = appendInput st x
                      | q4 fm ni = flip putInput x $ appendFM st 
                      | q2 fm ni = st{formula = "", newInputs = fm <> show x} -- レア
                      | otherwise = appendInput st x
                in nextState
              else -- そうでなければ数はNumberとして処理される。
                let nextState 
                      | q1' fm ni = appendInput st x
                      | q2' fm ni = appendInput st x
                      | q5' fm ni = st{formula = "",newInputs = fm <> show x} -- レアパターンなので直書き
                      | q9' fm ni = flip putInput x $ appendFM st
                      | q4' fm ni = appendInput st x
                      | q3' fm ni = appendInput st x
                      | q7' fm ni = appendInput st x
                      | q8' fm ni = appendInput st x
                      | q6' fm ni = appendInput st x
                      | otherwise = st
                in nextState
        AO x -> H.modify_ \st ->
          let fm = st.formula
              ni = st.newInputs
          in  let  putInput' = flip putInput
                   nextState
                      -- 以下、fmまたはniどちらかにピリオドが含まれる場合の遷移
                      | q1' fm ni = flip appendInput x $ resetNI $ appendFM $ cutEndOfNI st
                      | q2' fm ni = putInput' x $ appendFM $ st
                      | q5' fm ni = putInput' x $ appendFM st
                      | q9' fm ni = putInput' x st 
                      | q3' fm ni = putInput' x $ evalState st
                      | q6' fm ni = putInput' x $ evalState st
                      | q4' fm ni = putInput' x $ appendFM st
                      | q7' fm ni = putInput' x $ appendFM $ cutEndOfNI st
                      | q8' fm ni = putInput' x $ appendFM st
                      | q0 fm ni = flip putInput x $ appendFM st
                      | q1 fm ni = flip putInput x $ evalState st
                      | q5 fm ni = flip putInput x $ appendFM st
                      | q4 fm ni = putInput st x 
                      | q2 fm ni = putInput st x
                      | q3 fm ni = flip putInput x $ evalState st
                      | otherwise = st
              in nextState
        AS x -> H.modify_ \st -> 
            let fm = st.formula :: String
                ni = st.newInputs :: String
            in
                case x of 
                  Equal ->
                    let nextState
                          | q1' fm ni = cutEndOfNI st
                          | q2' fm ni = st
                          | q9' fm ni = st
                          | q3' fm ni = cutEndOfNI st
                          | q4' fm ni = resetNI $ appendFM st
                          | q5' fm ni = st
                          | q6' fm ni = resetNI $ evalState st
                          | q7' fm ni = resetNI $ evalState  st
                          | q8' fm ni = resetNI $ evalState st
                          | q0  fm ni = st
                          | q1 fm ni = st
                          | q2 fm ni = st
                          | q4 fm ni = st
                          | q3  fm ni = resetNI $ evalState st
                          | otherwise = evalState st
                    in nextState
                  AC    -> initialState_
                  Period-> 
                    let nextState
                          | q1' fm ni = cutEndOfNI st
                          | q2' fm ni = st
                          | q9' fm ni = st
                          | q3' fm ni = cutEndOfNI st
                          | q4' fm ni = st
                          | q5' fm ni = st
                          | q6' fm ni = st
                          | q7' fm ni = cutEndOfNI st
                          | q8' fm ni = appendInput st x
                          | q4 fm ni  = st
                          | q0 fm ni  = appendInput st x
                          | q1 fm ni  = appendInput st x
                          | q2 fm ni  = appendInput st x
                          | q3 fm ni  = appendInput st x 
                          | otherwise = st
                    in nextState
                  _     -> st
      
      
--           let fm = st.formula
--               ni = st.newInputs
--               evalFormula xs ys =
--                   let zs = fm <> ni
--                       zs' = runExprInt xs
--                   in case zs' of Left _  -> st
--                                  Right n -> st{formula = show n,newInputs = ""}
--           in  if fm == "" && ni == "0" 
--             then case _ of
--                     AN x -> st{newInputs = show x}
--                     AO x -> st{formula = ni, newInputs = show x}
--                     AS x ->
--                       case x of Period -> st{newInputs = ni <> "."}
--                                 AC     -> initialState 0
--                                 _      -> st
--             else if ni == "0" 
--               then case _ of
--                     AN x -> st{newInputs = show x}
--                     AO x -> (evalFormula fm ni){newInputs = show x}
--                     AS x -> 
--                       case x of Period -> st{newInputs = ni <> "."}
--                                 AC     -> initialState 0
--                                 Equal  -> evalFormula fm ni
--                                 _      -> st
--             else if ni == "+" || ni == "-" || ni == "×" || ni == "÷"
--               then case _ of
--                     AN x -> st{formula = fm <> ni, newInputs = show x}
--                     AO x -> st{newInputs = show x}
--                     AS x -> 
--                       case x of AC -> initialState 0
--                                 _  -> st
--               else case _ of 
--                     AN x -> st{newInputs = ni <> show x}
--                     AO x -> (evalFormula fm ni){newInputs = show x}
--                     AS x ->
--                       case x of Period -> st{newInputs = ni <> "."}
--                                 AC     -> initialState 0
--                                 Equal  -> evalFormula fm ni 
--                                 _      -> st
      
      
  --     let evalFormula st fm = if st.isNumber then show $ fromRight 0.0 $ runExprNumber fm else show $ fromRight 0 $ runExprInt fm in 
  -- case _ of
  --   AN x -> 
  --     let setFormula x = H.modify_ \st ->
  --           let fm = st.formula
  --               fm' = if fm == "0" then show x else fm <> show x
  --           in st{formula = fm'}
  --         last xs = let n = length xs in drop (n-1) xs 
  --     in case x of
  --           Zero  -> H.modify_ \st ->
  --             let fm = st.formula
  --                 c  = last fm
  --                 fm' = if fm == "0" then fm else fm <> "0"
  --             in  st{formula=fm'}
  --           One   -> setFormula One
  --           Two   -> setFormula Two
  --           Three -> setFormula Three
  --           Four  -> setFormula Four
  --           Five  -> setFormula Five
  --           Six   -> setFormula Six
  --           Seven -> setFormula Seven
  --           Eight -> setFormula Eight
  --           Nine  -> setFormula Nine
  --   AO x ->
  --     let init xs = let n = length xs in take (n-1) xs 
  --         processOperator st op = 
  --           let fm = st.formula
  --               flagEval = st.isAbleToEvaluate
  --               fm' = if flagEval then evalFormula st fm <> show op else (init fm) <> show op
  --           in st{formula = fm', isAbleToEvaluate = false,isPeriod = false} -- 演算子が一個ついた時点でisAbleToEvaluateはfalseにする
  --     in case x of
  --         Plus   -> H.modify_ \st ->
  --           let fm = st.formula
  --               fm' = if st.isAbleToEvaluate -- flagEvalがfalseになるのは、式の末尾が演算子のとき。その時に+が押されたら末尾を+に付け替える。
  --                       then evalFormula st fm <> "+"
  --                       else (init fm) <> "+"
  --           in st{formula = fm', isAbleToEvaluate = false, isPeriod = false}
  --         Minus  -> H.modify_ \st -> processOperator st Minus
  --         Times  -> H.modify_ \st -> processOperator st Times
  --         Divide -> H.modify_ \st -> processOperator st Divide
  --   AS x ->
  --     case x of C -> H.modify_ \_ -> initialState 0
  --               AC -> H.modify_ \st -> initialState 0
  --               Period -> H.modify_ \st -> 
  --                 let xs = st.formula
  --                     xs' = if st.isAbleToEvaluate && not (contains (Pattern ".") xs) then xs <> "." else xs
  --                 in if not st.isAbleToEvaluate then st{formula = xs} else st{formula = xs',isPeriod = true,isNumber = true} -- Periodがついた場合、それ以降はNumberの計算になる。ACを押されるとリセットされる。
  --               Equal -> H.modify_ \st -> if st.isAbleToEvaluate
  --                                           then if st.isNumber 
  --                                             then 
  --                                               let      
  --                                                 fm = st.formula
  --                                                 fm' = show $ fromRight 0.0 $ runExprNumber fm
  --                                               in st{formula = show fm'}
  --                                             else
  --                                               let 
  --                                                 fm = st.formula
  --                                                 fm' = show $ fromRight 0 $ runExprInt fm
  --                                               in st{formula = show fm'}
  --                                           else st
  --               _ -> H.modify_ \st -> st
  -- -- MakeRequest event -> do
  --   H.liftEffect $ Event.preventDefault event
  --   username <- H.gets _.username
  --   H.modify_ _ {loading = true}
  --   response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
  --   H.modify_ _ {loading = false, result = map _.body (hush response)}

-- handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
-- handleAction = case _ of
--   SetUsername username -> do
--     H.modify_ _ {username = username, result = Nothing}
--   MakeRequest event -> do
--     H.liftEffect $ Event.preventDefault event
--     username <- H.gets _.username
--     H.modify_ _ {loading = true}
--     response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
--     H.modify_ _ {loading = false, result = map _.body (hush response)}


-- type State =
--   {
--     loading  :: Boolean
--   , username :: String
--   , result   :: Maybe String
--   }

-- data Action
--   = SetUsername String
--   | MakeRequest Event


  -- HH.form
  --   [
  --     HE.onSubmit \ev -> MakeRequest ev
  --   , HP.class_ (ClassName "main-content")
  --   ]
  --   [
  --     HH.h1_ [HH.text "Look up GitHub user"] 
  --   , HH.label
  --       [
  --         HP.class_ (ClassName "btn")
  --       ]
  --       [
  --         HH.div_ [HH.text "Enter username:"]
  --       , HH.input
  --           [
  --             HP.value st.username
  --           , HE.onValueInput \str -> SetUsername str
  --           ]
  --       ]
  --   , HH.button
  --       [
  --         HP.disabled st.loading
  --       , HP.type_ HP.ButtonSubmit
  --       , HP.draggable true
  --       ]
  --       [HH.text "Fetch info"]
  --   , HH.p
  --       [HP.class_ (ClassName "main")]
  --       [HH.text $ if st.loading then "Working..." else ""]
  --   , HH.div
  --       [HP.class_ $ ClassName "display-area"]
  --       case st.result of
  --         Nothing  -> []
  --         Just res ->
  --           [
  --             HH.h2_
  --               [HH.text "Response:"]
  --           , HH.pre_
  --               [HH.code_ [HH.text res]]
  --           ]
  --   , stylesheet $ do
  --               expo
  --               Config.expo}
  --   ]




-- import Prelude

-- import Data.Maybe (Maybe(..), maybe)
-- import Data.Either(hush)
-- import Effect (Effect)
-- import Effect.Class (class MonadEffect)
-- import Effect.Console (log)
-- import Effect.Random (random)
-- import Halogen (ClassName(..))
-- import Halogen as H
-- import Halogen.Aff (awaitBody, runHalogenAff) as HA
-- import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.VDom.Driver (runUI)

-- main :: Effect Unit
-- main = do
--   let y = show $ test 2000
--   log $ "🍝 × " <>  y 
--   HA.runHalogenAff do
--     body <- HA.awaitBody
--     runUI component unit body

-- test :: Int -> Int
-- test x = 2 * x

-- -- Chapter 2
-- -- type State = Int

-- -- data Action = Increment | Decrement
-- -- component :: forall query input output m. H.Component query input output m
-- -- component =
-- --     H.mkComponent
-- --        { initialState
-- --         , render
-- --         , eval: H.mkEval H.defaultEval { handleAction = handleAction }
-- --        }
-- --     where
-- --     initialState _ = 0

-- --     render state =
-- --         HH.div_
-- --             [ HH.button [HE.onClick \_ -> Decrement ] [ HH.text "-"]
-- --             , HH.text( show state)
-- --             , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+"]
-- --             ]

-- --     handleAction = case _ of
-- --         Decrement ->
-- --             H.modify_ \state -> state - 1
-- --         Increment ->
-- --             H.modify_ \state -> state + 1

-- -- Chapter 3
-- type State = Maybe String

-- data Action = Regenerate String

-- component :: forall query input output m . MonadEffect m => H.Component query input output m
-- component =
--   H.mkComponent
--     {
--         initialState
--     ,   render
--     ,   eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
--     }

-- initialState :: forall input. input -> State
-- initialState _ = Nothing

-- render :: forall m. State -> H.ComponentHTML Action () m
-- render state = do
--   let value = maybe "No number generated yet" identity state
--   let divBtn y = HH.div [HP.class_ (ClassName "btn"),HE.onClick \_ -> Regenerate (value <> y)] [HH.text y]
--   HH.div_
--     [
--         HH.h1_
--           [ HH.text "Random number" ]
--     ,   HH.p_
--           [ 
--             HH.text ("Current value: " <> value)
--           ]
--     ,   HH.button
--           [ HE.onClick \_ -> Regenerate $ value <> "Test"]
--           [ HH.text "Generate new number" ]
--     ,   HH.li_
--             [
                
--             ]
--     ,   HH.div [HP.class_ $ ClassName "main-context"]
--           [
--             HH.div [HP.class_ $ ClassName "content-area"]
--               [
--                 HH.div [HP.class_ $ ClassName "display-area"]
--                   [
--                       HH.div [HP.id "numdisplay"] [HH.text $ value]
--                     , HH.div [HP.id "howcal_display"] []
--                   ]
--               , HH.div [HP.class_ $ ClassName "click-area"]
--                 [
--                     divBtn  "+"
--                   , divBtn  "-"
--                   , divBtn  "×"
--                   , divBtn  "÷"
--                   , divBtn  "AC"
--                 ]
--               ]
--           ]
--     ]
    

-- handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
-- handleAction = case _ of
--     Regenerate xs -> do
--         --newNumber <- H.liftEffect random
--         H.modify_ \_ -> Just xs