module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Chat as Chat
import Home as Home
import Prompt as Prompt
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, proxy, runStorybook)


main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook { stories
                                , logo: Just (HH.text "ami")
                                }
  where stories :: Stories Aff
        stories = Object.fromFoldable [ Tuple ""       $ proxy Home.component
                                      , Tuple "chat"   $ proxy Chat.component
                                      , Tuple "prompt" $ proxy Prompt.component
                                      ]
