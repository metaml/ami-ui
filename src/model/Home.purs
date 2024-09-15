module Home where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

component :: _ _ _ Aff
component = H.mkComponent { initialState: identity
                          , render: \_ -> render, eval: H.mkEval H.defaultEval
                          }

render :: H.ComponentHTML _ () Aff
render = HH.article_ [ HH.h3_ [ HH.text "ami" ]           -- title
                     , HH.p_  [ HH.text "ami operations" ] -- description
                     ]
