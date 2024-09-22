module PromptState where

import Prelude
import Halogen as H
import Halogen.HTML as HH

type Slot p = forall q. H.Slot q Void p
