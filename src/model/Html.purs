module Html where

import Prelude
import DOM.HTML.Indexed (CSSPixel, HTMLinput(..))
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

mkComponent :: forall q i o result. Show result
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

width :: forall r i. CSSPixel -> HP.IProp (width :: CSSPixel | r) i
width = HP.prop (PropName "width")

type Checkbox error action =
  { label :: String
  , state :: FieldState Boolean error Boolean
  , action :: FieldAction action Boolean error Boolean
  }

checkbox :: forall error action slots m.
            Checkbox error action
         -> Array (HP.IProp HTMLinput action)
         -> H.ComponentHTML action slots m
checkbox { label, state, action } props =
  HH.fieldset_ [ HH.label_
                 [ HH.input $ flip append props
                   [ HP.type_ HP.InputCheckbox
                   , HP.checked state.value
                   , HE.onChecked action.handleChange
                   , HE.onBlur action.handleBlur
                   ]
                 , HH.text label
                 ]
               ]

checkbox_ :: forall error action slots m.
             Checkbox error action
          -> H.ComponentHTML action slots m
checkbox_ = flip checkbox []
