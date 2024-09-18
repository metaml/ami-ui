module Html where

import Prelude
import DOM.HTML.Indexed (CSSPixel) as I
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Formless (FieldAction, FieldState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName, Namespace, PropName(..), Prop)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Title = String
type Description = String

mkComponent :: forall q i o result . Show result
            => Title
            -> Description
            -> H.Component q Unit result Aff
            -> H.Component q i o Aff
mkComponent title descr formComponent =
  H.mkComponent { initialState: \_ -> { result: Nothing }
                , render
                , eval: H.mkEval $ H.defaultEval { handleAction = action }
                }
  where action result = H.modify_ _ { result = Just result }
        render state = HH.article_ [ HH.h3_ [ HH.text title ]
                                   , HH.p_ [ HH.text descr ]
                                   , HH.slot (Proxy :: Proxy "inner") unit formComponent unit identity
                                   , case state.result of
                                       Nothing -> HH.text ""
                                       Just result -> HH.code_ [ HH.text $ show result ]
                                   ]

width :: forall r i. I.CSSPixel -> HP.IProp (width :: I.CSSPixel | r) i
width = HP.prop (PropName "width")
