module Counter where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Int
data Action = Increment | Decrement

component :: forall query input output m. H.Component query input output m
component = H.mkComponent { initialState
                          , render
                          , eval: H.mkEval H.defaultEval { handleAction = action }
                          }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_ [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
                       , HH.text (show state)
                       , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
                       ]

action :: forall output m. Action -> H.HalogenM State Action () output m Unit
action = case _ of
  Decrement ->
    H.modify_ \state -> state - 1
  Increment ->
    H.modify_ \state -> state + 1
