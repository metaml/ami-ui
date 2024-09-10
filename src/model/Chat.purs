module Chat where

import Prelude
import Data.Array
import Data.Argonaut as Argo
import Data.Const
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Console (log, logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HE
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f = ( name    :: f String String String
              , message :: f String String String
              )           -- input  error  output

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

type Message = { | Form F.FieldOutput }
type State = { context :: FormContext, messages :: Array Message }

data Action = Receive FormContext | Eval FormlessAction
type FormInputs = { | Form F.FieldInput }


-- type Query = Const Void
type Input = Unit
type Output = { | Form F.FieldOutput }

form :: forall query. H.Component query Input Output Aff
form = F.formless { liftAction: Eval } mempty
       $ H.mkComponent { initialState: \ctx -> { context: ctx, messages: [] }
                       , render
                       , eval: H.mkEval $ H.defaultEval
                         { receive = Just <<< Receive
                         , handleAction = action
                         , handleQuery  = query
                         }
                       }

action :: forall m. MonadEffect m
          => Action
          -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m Unit
action = case _ of
           Receive context -> H.modify_ _ { context = context }
           Eval action'    -> F.eval action'

query :: forall a m. MonadEffect m => F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
query = do
  let onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
      onSubmit fields = do
        state <- H.get
        H.modify_ _ { messages = state.messages
                                 <> [{ message: fields.message, name: fields.name } :: Message]
                    }
      validation :: { | Form F.FieldValidation }
      validation = { name: case _ of
                       "" -> Left "a name is required"
                       nom -> Right nom
                   , message: case _ of
                       ""  -> Left "a message is required!"
                       msg -> Right msg
                   }
  F.handleSubmitValidate onSubmit F.validate validation

render :: State -> H.ComponentHTML Action () Aff
render { context: { formActions, fields, actions}, messages } = do
  let lines = intersperse HH.br_ $ map (\{ name, message } -> HH.text (name <> ": " <> message)) messages
  HH.form [ HEV.onSubmit formActions.handleSubmit ]
          [ HH.div_ [ HH.label_ []
                    , HH.pre_ lines
                    ]
          ,  HH.div_ [ HH.label_ []
                     , HH.input [ HP.type_ HP.InputText
                                , HEV.onValueInput actions.message.handleChange
                                , HEV.onBlur actions.message.handleBlur
                                , case fields.message.result of
                                    Nothing        -> HP.placeholder "message"
                                    Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                    Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                                ]
                     ]
          ,  HH.div_ [ HH.label_ []
                     , HH.input [ HP.type_ HP.InputText
                                , HEV.onValueInput actions.name.handleChange
                                , HEV.onBlur actions.name.handleBlur
                                , case fields.name.result of
                                    Nothing        -> HP.placeholder "name"
                                    Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                    Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                                ]
                     ]
          , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Submit" ]
          ]

-- toTr :: Message -> Array HH.HTML
-- toTr { name, message } = HE.tr_ [ HE.td_ [HH.text name]
--                                 , HE.td_ [HH.text message]
--                                 ]
