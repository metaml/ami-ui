module Main where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Chat as Chat
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, proxy, runStorybook)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Just (HH.text "ami")
    }

stories :: Stories Aff
stories = Object.fromFoldable [ home
                              , chat
                              ]
  where
    home = do
      let render = HH.article_ [ HH.h3_ [ HH.text "ami" ]           -- title
                               , HH.p_ [ HH.text "ami operations" ] -- description
                               ]
          component = H.mkComponent { initialState: identity
                                    , render: \_ -> render, eval: H.mkEval H.defaultEval
                                    }
      Tuple "" $ proxy component
    chat = Tuple "chat" $ proxy $ mkComponent "chat" "" Chat.form

type Title = String
type Description = String

mkComponent :: forall q i o result . Show result
            => Title
            -> Description
            -> H.Component q Unit result Aff
            -> H.Component q i o Aff
mkComponent title descr formComponent = H.mkComponent
  { initialState: \_ -> { result: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = action }
  }
  where action result = H.modify_ _ { result = Just result }
        render state = HH.article_ [ HH.h3_ [ HH.text title ]
                                   , HH.p_ [ HH.text descr ]
                                   , HH.slot (Proxy :: Proxy "inner") unit formComponent unit identity
                                   , case state.result of
                                       Nothing -> HH.text ("no state: " <> show state)
                                       Just result -> HH.code_ [ HH.text $ show result ]
                                   ]
