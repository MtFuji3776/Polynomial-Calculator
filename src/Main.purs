module Main where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class(class MonadAff)
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
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import StyleSheet(expo)
import Halogen.HTML.CSS
import Config(expo) as Config


--import Web.HTML.HTMLDocument (body)




main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
  {
    loading  :: Boolean
  , username :: String
  , result   :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest Event

component :: forall query input output m . MonadAff m => H.Component query input output m
component =
  H.mkComponent
    {
      initialState
    , render
    , eval : H.mkEval $ H.defaultEval {handleAction = handleAction}
    } -- レコード型の一種だろうか？これを引数にとってcomponentが生成される。
  
initialState :: forall input . input -> State
initialState _ = {loading: false, username: "", result: Nothing}

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
                  let divBtn xs = HHE.div [HP.class_ $ ClassName "btn"] [HH.text xs]
                  in
                  HHE.div [HP.class_ $ ClassName "content-area"]
                  [
                    HHE.div [HP.class_ $ ClassName "display-area"]
                      [
                        HHE.div [HP.class_ $ ClassName "num_display"]
                        [
                          HH.text "0"
                        ]
                      , HHE.div [HP.class_$ ClassName "howcal_display"] []
                      ]
                  , HHE.div [HP.class_ $ ClassName "click-area"]
                    [
                        HHE.div [HP.class_ $ ClassName "operator"]
                        [
                          divBtn "+"
                        , divBtn "-"  
                        , divBtn "×"
                        , divBtn "÷"
                        , divBtn "AC"
                        ]
                      , HHE.div [classname "operator"]
                        [
                          HHE.div [classname "row1"]
                            [
                              divBtn "7"
                            , divBtn "8"
                            , divBtn "9"
                            ]
                        , HHE.div [classname "row2"]
                            [
                              divBtn "4"
                            , divBtn "5"
                            , divBtn "6"
                            ]
                        , HHE.div [classname "row3"]
                            [
                              divBtn "1"
                            , divBtn "2"
                            , divBtn "3"
                            ]
                        , HHE.div [classname "row4"]
                            [
                              divBtn "0"
                            , divBtn "."
                            , divBtn "C"
                            ]
                        , HHE.div [classname "row5"]
                            [
                              divBtn "="
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

handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ {username = username, result = Nothing}
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ {loading = true}
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ {loading = false, result = map _.body (hush response)}



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