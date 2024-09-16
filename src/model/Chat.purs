module Chat where

import Prelude
import Ami as A
import Data.Array (head)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log, logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP
import Html as Html
import Web.HTML.Common (ClassName(..))

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

type Query :: forall k. k -> Type
type Query = Const Void -- forall k. k -> Type
type Input = Unit -- { | Form F.FieldInput }
type Output = { | Form F.FieldOutput }

component :: _ _ _ Aff
component = Html.mkComponent "chat" "" form

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

query :: forall a m. MonadAff m => F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
query = do
  let onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
      onSubmit fields = do
        state' <- H.get
        H.modify_ _ { messages = state'.messages <> [ { message: fields.message
                                                      , name: fields.name
                                                      }
                                                    ]
                    }

        let msg :: A.Message
            msg = { content: fields.message, role: "user" }
            req :: A.Request
            req = { messages: [ msg ], stream: false }
        { messages, friend } :: A.Response <- H.liftAff $ A.talk req

        let msg' = head messages
        case msg' of
          Nothing  -> do
            H.liftEffect $ log "Nothing"
            pure unit
          Just amiMsg -> do
            let rec = { message: amiMsg.content, name: friend } :: Message
            H.liftEffect $ logShow rec
            state <- H.get
            H.modify_ _ { messages = state.messages <> [rec] }

      validation :: { | Form F.FieldValidation }
      validation = { name: case _ of
                       ""  -> Left "a name is required"
                       nom -> Right nom
                   , message: case _ of
                       ""  -> Left "a message is required"
                       msg -> Right msg
                   }
  F.handleSubmitValidate onSubmit F.validate validation

render :: State -> H.ComponentHTML Action () Aff
render { context: { formActions, fields, actions}, messages } = do
  -- let lines = intersperse HH.br_ $ map (\{ name, message } -> HH.text (name <> ": " <> message)) messages
  let texts = (\{ name, message } -> HH.text (name <> ": " <> message)) <$> messages
      container = HP.class_ (ClassName "container")
      articles = (\text -> HH.article [ container ] [ text ]) <$> texts
  HH.form [ HEV.onSubmit formActions.handleSubmit ]
          [ HH.div_ [ HH.label_ []
                    , HH.ul_ articles
                    ]
          , HH.div_ [ HH.label_ []
                    , HH.input [ HP.type_ HP.InputText
                               , HEV.onValueInput actions.message.handleChange
                               , HEV.onBlur actions.message.handleBlur
                               , case fields.message.result of
                                   Nothing        -> HP.placeholder "message"
                                   Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                   Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                               ]
                    ]
          , HH.div_ [ HH.label_ []
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
