module Chat where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f = ( message :: f String String String )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action = Receive FormContext | Eval FormlessAction

form :: forall query. H.Component query Unit { | Form F.FieldOutput } Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = action
      , handleQuery  = query
      }
  }
  where action :: Action -> H.HalogenM _ _ _ _ _ Unit
        action = case _ of
          Receive context -> H.put context
          Eval action'    -> F.eval action'
        query :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
        query = do
          let validation :: { | Form F.FieldValidation }
              validation = { message: case _ of
                                        ""  -> Left "a message is required"
                                        msg -> Right msg
                           }
          F.handleSubmitValidate F.raise F.validate validation

render :: FormContext -> H.ComponentHTML Action () Aff
render { formActions, fields, actions } = HH.form
  [ HE.onSubmit formActions.handleSubmit ]
  [ HH.div_ [ HH.label_ []
            , HH.input [ HP.type_ HP.InputText
                       , HE.onValueInput actions.message.handleChange
                       , HE.onBlur actions.message.handleBlur
                       , case fields.message.result of
                           Nothing        -> HP.placeholder "please type a message"
                           Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                           Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                       ]
            , case fields.message.result of
                Just (Left err) -> HH.small_ [ HH.text "i dunno" ]
                _ -> HH.text ""
            ]
  , HH.button [ HP.type_ HP.ButtonSubmit ]
              [ HH.text "Submit" ]
  ]


-- [ HH.label_ [ HH.text "Name" ]
-- , HH.input
--     [ HP.type_ HP.InputText
--     , HE.onValueInput actions.name.handleChange
--     , HE.onBlur actions.name.handleBlur
--     , case fields.name.result of
--         Nothing -> HP.placeholder "Jack"
--         Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
--         Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
--     ]
-- , case fields.name.result of
--     Just (Left err) -> HH.small_ [ HH.text err ]
--     _ -> HH.text ""
-- ]
