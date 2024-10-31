module Chat where

import Prelude
import Ami as Ami
import Data.Array ((..), head, length, null, zip)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Int as Int
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log, logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HEV
import Halogen.HTML.Elements as HE
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
            | Put Ami.Prompt Boolean
            | Receive FormContext
            | Eval FormlessAction
type FormInputs = { | Form F.FieldInput }

type Query :: forall k. k -> Type
type Query = Const Void
type Input = Unit -- { | Form F.FieldInput }
type Output = { | Form F.FieldOutput }

type PromptMap = M.Map Ami.Prompt Boolean

type Message = { | Form F.FieldOutput }
type State = { context :: FormContext
             , messages :: Array Message
             , member :: String
             , friend :: String
             , promptMap :: PromptMap
             }

component :: _ _ _ Aff
component = Html.mkComponent "chat" "" form

form :: forall query. H.Component query Input Output Aff
form = F.formless { liftAction: Eval } mempty
       $ H.mkComponent { initialState: \ctx -> { context: ctx
                                               , messages: []
                                               , member: ""
                                               , friend: "Courtney"
                                               , promptMap: M.empty
                                               }
                       , render
                       , eval: H.mkEval $ H.defaultEval { receive = Just <<< Receive
                                                        , initialize = Just Initialize
                                                        , handleAction = action
                                                        , handleQuery  = query
                                                        }
                       }

action :: forall m. MonadAff m
          => Action
          -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m Unit
action = case _ of
           Initialize -> do
             { context, messages, member, friend, promptMap } <- H.get
             messages <- H.liftAff (Ami.messages { member: member, friend: friend })
             prompts <- H.liftAff (Ami.prompts { member: "system", friend: "system" })
             let promptMap' = foldr insert M.empty prompts
             H.modify_ _ { context = context, messages = messages
                         , member = member
                         , friend = friend
                         , promptMap = promptMap'
                         }
             H.liftEffect $ logShow messages
             H.liftEffect $ logShow member
             H.liftEffect $ logShow friend
             H.liftEffect $ logShow promptMap'
           Put prompt bool -> do
             { context, promptMap } <- H.get
             let promptMap' = M.insert prompt bool promptMap
             H.modify_ _ { promptMap = promptMap' }
             b <- H.liftAff $ Ami.promptUpdate { prompt: prompt.prompt
                                               , member_id: prompt.member
                                               , friend_id: prompt.friend
                                               , enabled: bool
                                               }
             H.liftEffect $ logShow promptMap'
           Receive context -> H.modify_ _ { context = context }
           Eval action'    -> F.eval action'
        where insert :: Ami.Prompt -> PromptMap -> PromptMap
              insert p pmap = M.insert p p.enabled pmap

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
        { messages, friend } :: Ami.MsgRes <- H.liftAff $ Ami.talk req
        H.liftEffect $ logShow fields
        H.liftEffect $ logShow msg
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
render { context: { formActions, fields, actions }, messages, member, friend, promptMap } = do
  let dialog = (\{ name, message } -> (name <> ": " <> message)) <$> messages
      dialog' = foldr (\a b -> a <> "\n\n" <> b) "\n\n" dialog
      tuples = idxPrompts (S.toUnfoldable (M.keys promptMap))
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
                               , HEV.onValueInput actions.name.handleChange
                               , HEV.onBlur actions.name.handleBlur
                               , case fields.name.result of
                                   Nothing        -> HP.placeholder "name"
                                   Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                   Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                               ]
                    ]
          , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Submit" ]
          , HH.div_ [ HE.fieldset_
                      (
                        [ HE.legend_ [] ]
                        <> (inputCheckbox promptMap <$> tuples)
                      )
                    ]
          ]
  where
    idxPrompts :: Array Ami.Prompt -> Array (Tuple Int Ami.Prompt)
    idxPrompts prompts = zip (0..(length prompts)) prompts

    inputCheckbox :: M.Map Ami.Prompt Boolean
                     -> Tuple Int Ami.Prompt
                     -> HH.HTML (_ Aff Action) Action
    inputCheckbox promptMap (Tuple index prompt) =
      HH.label [ HP.for (toString index) ]
      [ HH.input [ HP.type_ HP.InputCheckbox
                 , HP.id (toString index)
                 , HP.name "checked"
                 , HP.value prompt.prompt
                 , HP.checked (checked prompt promptMap)
                 , HEV.onChecked \b -> (Put prompt b)
                 ]
      , HH.text prompt.prompt
      ]
      where toString :: Int -> String
            toString = Int.toStringAs Int.decimal
            checked :: Ami.Prompt -> PromptMap -> Boolean
            checked p pm = case M.lookup p pm of
              Nothing -> false
              Just b  -> b
