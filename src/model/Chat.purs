module Chat where

import Prelude
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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f = ( name :: f String String String
              , message :: f String String String
              )           -- input  error  output

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)
data Action = Receive FormContext | Eval FormlessAction

type FormInputs = { | Form F.FieldInput }
type State = FormContext

type Message = { | Form F.FieldOutput }
-- type Query = Const Void
type Input = Unit
type Output = { | Form F.FieldOutput }

form :: forall query. H.Component query Input Output Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent { initialState: identity
                                                              , render
                                                              , eval: H.mkEval $ H.defaultEval
                                                                  { receive = Just <<< Receive
                                                                  , handleAction = action
                                                                  , handleQuery  = query
                                                                  }
                                                              }

render :: FormContext -> H.ComponentHTML Action () Aff
render { formActions, fields, actions } = do
  HH.form [ HE.onSubmit formActions.handleSubmit ]
          [ HH.div_ [ HH.label_ []
                    , HH.pre [ HP.class_ $ HH.ClassName "chatbox"
                             , HP.id "chatbox"
                             ]
                             [ HH.text "foo bar baz" ]
                    ]
          ,  HH.div_ [ HH.label_ []
                     , HH.input [ HP.type_ HP.InputText
                                , HE.onValueInput actions.message.handleChange
                                , HE.onBlur actions.message.handleBlur
                                , case fields.message.result of
                                    Nothing        -> HP.placeholder "please type a message"
                                    Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                    Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                                ]
                     , case fields.message.result of
                         Nothing          -> HH.text ""
                         Just (Left err)  -> HH.small_ [ HH.text err ]
                         Just (Right msg) -> HH.text msg
                     ]
          , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Submit" ]
          ]

action :: forall m. MonadEffect m
          => Action
          -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m Unit
action = case _ of
           Receive context -> do
             let res = context.fields.message.result
                 req = case res of
                         Nothing           -> ""
                         Just (Left _)     -> ""
                         Just (Right line) -> line
             H.liftEffect $ log ("results=" <> show res <> "|req=" <> show req)
             H.put context
           Eval action' -> F.eval action'

query :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
query = do
  let validation :: { | Form F.FieldValidation }
      validation = { name: case _ of
                       "" -> Left "a name is required"
                       nom -> Right nom
                   , message: case _ of
                       ""  -> Left "a message is required"
                       msg -> Right msg
                   }
      success :: Message -> H.HalogenM _ _ _ _ _ Unit
      success msg = do
        let output :: Output
            output = { name: msg.name, message: msg.message }
        F.raise output
  F.handleSubmitValidate success F.validate validation
  -- F.handleSubmitValidate F.raise F.validate validation -- without "success"
