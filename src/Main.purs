module Main where

import Data.Boolean
import Halogen.HTML.CSS
import Parser
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Config (expo) as Config
import Data.Either (Either(..), fromRight)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (singleton, uncons, length, take, drop, contains, Pattern(..))
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

-- import Data.Array(cons)

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


data Action = AN ActNum | AO ActOperator | AS ActSpecial

type State = { formula :: String
             , newInputs :: String
            --  , inputLog :: Array {formula_::String, newInputs_ :: String}
            --  , counter :: Int
            --  , evalLog :: Array Int
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
                 , newInputs: "0"
                --  , inputLog:  []
                --  , counter:    0
                --  , evalLog:   []
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

q0 x y = x == "" && y == "0"

q1 x y = x == "" && (not (y == "" || y == "0"))

q2 x y = (not (x == "")) && y == ""

q3 x y = not (x == "" || y == "")

q4 x y = (not (x == "")) && (y == "+" || y == "-" || y == "×" || y == "÷")

evalFormula :: State -> State
evalFormula st =
    let fm = st.formula
        ni = st.newInputs
        zs = fm <> ni
        zs' = runExprInt zs
    in case zs' of Left _  -> st
                   Right n -> st{formula = show n,newInputs = ""}
-- なんと、handleActionをよく見れば、Actionを引数に取る関数である。
  -- StateとActionをオートマトンと見做す時、handleActionは遷移関数に相当するが、その定義を入力文字別に場合分けして書き下すことが義務付けられているということ。
  -- 正直Stateベースで場合分けする方が書きやすい…
handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction  = case _ of 
        AN x -> H.modify_ \st -> 
          let fm = st.formula
              ni = st.newInputs
          in if q0 fm ni then st {newInputs = show x}
              else if q1 fm ni then st{newInputs = (ni <> show x)}
              else if q4 fm ni then st{formula = (evalFormula st{formula = fm <> ni}).formula,newInputs = show x} 
              else if q2 fm ni then st{formula = "",newInputs = (fm <> show x)}
              else  st{newInputs = ni <> show x}
        AO x -> H.modify_ \st ->
          let fm = st.formula
              ni = st.newInputs
          in if q0 fm ni then st{formula = show $ runExprInt ni,newInputs = show x}
              else if q1 fm ni then st{formula = (evalFormula st).formula,newInputs = show x}
              else if q4 fm ni then st{newInputs = show x}
              else if q2 fm ni then st{newInputs = show x}
              else st{newInputs = (ni <> show x)}
        AS x -> H.modify_ \st -> 
            let fm = st.formula :: String
                ni = st.newInputs :: String
            in
                case x of 
                  Equal ->
                         if q0 fm ni then st 
                          else if q1 fm ni then st
                          else if q4 fm ni then st
                          else if q2 fm ni then st
                          else st{formula = (evalFormula st).formula,newInputs = ""}
                  AC    -> initialState_
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