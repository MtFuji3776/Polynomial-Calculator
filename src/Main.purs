module Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Random (random)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff) as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLDocument (body)



main :: Effect Unit
main = do
  let y = show $ test 2000
  log $ "🍝 × " <>  y 
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

test :: Int -> Int
test x = 2 * x

-- Chapter 2
-- type State = Int

-- data Action = Increment | Decrement
-- component :: forall query input output m. H.Component query input output m
-- component =
--     H.mkComponent
--        { initialState
--         , render
--         , eval: H.mkEval H.defaultEval { handleAction = handleAction }
--        }
--     where
--     initialState _ = 0

--     render state =
--         HH.div_
--             [ HH.button [HE.onClick \_ -> Decrement ] [ HH.text "-"]
--             , HH.text( show state)
--             , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+"]
--             ]

--     handleAction = case _ of
--         Decrement ->
--             H.modify_ \state -> state - 1
--         Increment ->
--             H.modify_ \state -> state + 1

-- Chapter 3
type State = Maybe String

data Action = Regenerate String

component :: forall query input output m . MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    {
        initialState
    ,   render
    ,   eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let value = maybe "No number generated yet" identity state
  let divBtn y = HH.div [HP.class_ (ClassName "btn"),HE.onClick \_ -> Regenerate (value <> y)] [HH.text y]
  HH.div_
    [
        HH.h1_
          [ HH.text "Random number" ]
    ,   HH.p_
          [ 
            HH.text ("Current value: " <> value)
          ]
    ,   HH.button
          [ HE.onClick \_ -> Regenerate $ value <> "Test"]
          [ HH.text "Generate new number" ]
    ,   HH.li_
            [
                
            ]
    ,   HH.div [HP.class_ $ ClassName "main-context"]
          [
            HH.div [HP.class_ $ ClassName "content-area"]
              [
                HH.div [HP.class_ $ ClassName "display-area"]
                  [
                      HH.div [HP.id "numdisplay"] [HH.text $ value]
                    , HH.div [HP.id "howcal_display"] []
                  ]
              , HH.div [HP.class_ $ ClassName "click-area"]
                [
                    divBtn  "+"
                  , divBtn  "-"
                  , divBtn  "×"
                  , divBtn  "÷"
                  , divBtn  "AC"
                ]
              ]
          ]
    ]
    

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Regenerate xs -> do
        --newNumber <- H.liftEffect random
        H.modify_ \_ -> Just xs