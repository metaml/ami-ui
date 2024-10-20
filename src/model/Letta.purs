module Letta where

import Prelude
import Ami as Ami
import Data.Array (head, null)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldr)
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
import Undefined (undefined)

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f = ( message :: f String String String
              , name    :: f String String String
              )           -- input  error  output

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action = Initialize
            | Receive FormContext
            | Eval FormlessAction
type FormInputs = { | Form F.FieldInput }

type Query :: forall k. k -> Type
type Query = Const Void
type Input = Unit -- { | Form F.FieldInput }
type Output = { | Form F.FieldOutput }

type Message = { | Form F.FieldOutput }
type State = { context :: FormContext
             , messages :: Array Message
             , member :: String
             , friend :: String
             }

component :: _ _ _ Aff
component = Html.mkComponent "letta" "" form

form :: forall query. H.Component query Input Output Aff
form = F.formless { liftAction: Eval } mempty
       $ H.mkComponent { initialState: \ctx -> { context: ctx
                                               , messages: []
                                               , member: "Chad"
                                               , friend: "Courtney"
                                               }
                       , render
                       , eval: H.mkEval $ H.defaultEval
                         { receive = Just <<< Receive
                         , handleAction = action
                         , handleQuery  = query
                         }
                       }

action :: forall m. MonadAff m
          => Action
          -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m Unit
action = case _ of
           Initialize -> do
             { context, messages, member, friend } <- H.get
             messages <- H.liftAff (Ami.messages { member: member, friend: friend })
             H.liftEffect $ logShow messages
             H.liftEffect $ logShow member
             H.liftEffect $ logShow friend
             H.modify_ _ { context = context, messages = messages, member = member, friend = friend }
           Receive context -> H.modify_ _ { context = context }
           Eval action'    -> F.eval action'

query :: forall a m. MonadAff m => F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
query = do
  let onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
      onSubmit fields = do
        state' <- H.get
        messages <- if null state'.messages
                    then H.liftAff (Ami.messages { member: fields.name
                                                 , friend: state'.friend
                                                 }
                                   )
                    else pure state'.messages
        H.liftEffect $ logShow messages
        H.modify_ _ { messages = messages <> [ { message: fields.message
                                               , name: fields.name
                                               }
                                             ]
                    , member = fields.name
                    }
        let msg :: Ami.Msg
            msg = { content: fields.message, role: "user", member: fields.name, friend: state'.friend }
            req :: Ami.MsgReq
            req = { messages: [ msg ], stream: false }
        H.liftEffect $ logShow fields
        H.liftEffect $ logShow msg
        { messages, friend } :: Ami.MsgRes <- H.liftAff $ Ami.lettaTalk req

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
                       ""  -> Right "Chad"
                       nom -> Right nom
                   , message: case _ of
                       ""  -> Left "a message is required"
                       msg -> Right msg
                   }
  F.handleSubmitValidate onSubmit F.validate validation

render :: State -> H.ComponentHTML Action () Aff
render { context: { formActions, fields, actions }, messages, member, friend } = do
  let dialog = (\{ name, message } -> (name <> ": " <> message)) <$> messages
      dialog' = foldr (\a b -> a <> "\n\n" <> b) "\n\n" dialog
  HH.form [ HEV.onSubmit formActions.handleSubmit ]
          [ HH.div_ [ HH.label_ []
                    , HH.textarea [ HP.rows 13
                                  , HP.readOnly true
                                  , HP.value dialog'
                                  ]
                    ]
          , HH.div_ [ HH.label_ []
                    , HH.textarea [ HP.autofocus true
                                  , HP.rows 3
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
                               , HP.value "Chad"
                               , HEV.onValueInput actions.name.handleChange
                               , HEV.onBlur actions.name.handleBlur
                               , case fields.name.result of
                                   Nothing        -> HP.placeholder "Chad"
                                   Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                   Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                               ]
                    ]
          , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Submit" ]
          ]
