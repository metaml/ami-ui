module Prompt where

import Prelude
import DOM.HTML.Indexed (HTMLinput(..))
import Data.Array ((..), concat, length, zip)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable) as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log, logShow)
import Formless (FieldAction, FieldState)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements as HE
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP
import Html as Html
import PromptState as P
import Undefined (undefined)

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f = ( prompt  :: f String String String
              , member  :: f String String String
              , friend  :: f String String String
              )           -- input  error  output

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

type PromptMap = M.Map Prompt Boolean

data Action = Initialize
            | Put Prompt Boolean
            | Receive FormContext
            | Eval FormlessAction
type FormInputs = { | Form F.FieldInput }

type Query :: forall k. k -> Type
type Query = Const Void -- forall k. k -> Type
type Input = Unit -- { | Form F.FieldInput }
type Output = { | Form F.FieldOutput }

type Prompt = { | Form F.FieldOutput }

type State = { context :: FormContext
             , prompts :: PromptMap
             }

-- type Slots = ( put :: P.Slot PromptMap )

component :: _ _ _ Aff
component = Html.mkComponent "prompt" "" form

form :: forall query. H.Component query Input Output Aff
form = F.formless { liftAction: Eval } initialForm
       $ H.mkComponent { initialState: \context -> { context: context, prompts: M.empty }
                       , render
                       , eval: H.mkEval $ H.defaultEval { receive = Just <<< Receive
                                                        -- , initialize = Just Initialize
                                                        , handleAction = action
                                                        , handleQuery  = query
                                                        }
                       }
       where initialForm :: { | Form F.FieldInput }
             initialForm = { prompt: "", member: "", friend: "" }


-- slots can equal ()
action :: forall slots m. MonadEffect m
          => Action
          -> H.HalogenM State Action slots (F.FormOutput (Form F.FieldState) Output) m Unit
action = case _ of
           Initialize      -> initialize
           Put prompt bool -> do { context, prompts } <- H.get
                                 H.modify_ _ { prompts = M.insert prompt bool prompts }
           Receive context -> H.modify_ _ { context = context }
           Eval action'    -> F.eval action'

query :: forall a m. MonadAff m => F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
query = do
  let onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ _ Unit
      onSubmit fields = do
        state' <- H.get
        H.liftEffect $ logShow fields
        H.modify_ _ { prompts = M.insert { prompt: fields.prompt
                                         , member: fields.member
                                         , friend: fields.friend
                                         }
                                         true
                                         state'.prompts
                    }
      validation :: { | Form F.FieldValidation }
      validation = { prompt: case _ of
                       ""  -> Left "a member is required"
                       pr -> Right pr
                   , member: case _ of
                       ""  -> Left "a prompt is required"
                       mb -> Right mb
                   , friend: case _ of
                       ""  -> Left "a friend is required"
                       fr -> Right fr
                   }
  F.handleSubmitValidate onSubmit F.validate validation

render :: State -> H.ComponentHTML Action () Aff
render { context: { formActions, fields, actions}, prompts } = do
  let tuples = idxPrompts (S.toUnfoldable (M.keys prompts))
  HH.form [ HEV.onSubmit formActions.handleSubmit ]
          [ HH.div_ [ HE.fieldset_
                      (
                        [ HE.legend_ [ HH.text "properties" ] ]
                        <> (inputCheckbox <$> tuples)
                      )
                    ]
          , HH.div_ [ HH.label_ []
                    , HH.input [ HP.type_ HP.InputText
                               , HEV.onValueInput actions.prompt.handleChange
                               , HEV.onBlur actions.prompt.handleBlur
                               , case fields.prompt.result of
                                   Nothing        -> HP.placeholder "prompt"
                                   Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                   Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                               ]
                    ]
          , HH.div_ [ HH.label_ []
                    , HH.input [ HP.type_ HP.InputText
                               , HEV.onValueInput actions.member.handleChange
                               , HEV.onBlur actions.member.handleBlur
                               , case fields.member.result of
                                   Nothing        -> HP.placeholder "member"
                                   Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                   Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                               ]
                    ]
          , HH.div_ [ HH.label_ []
                    , HH.input [ HP.type_ HP.InputText
                               , HEV.onValueInput actions.friend.handleChange
                               , HEV.onBlur actions.friend.handleBlur
                               , case fields.friend.result of
                                   Nothing        -> HP.placeholder "friend"
                                   Just (Left _)  -> HP.attr (HH.AttrName "aria-invalid") "true"
                                   Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                               ]
                    ]
          , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Submit" ]
          ]

    where
      idxPrompts :: Array Prompt -> Array (Tuple Int Prompt)
      idxPrompts prompts = zip (0..(length prompts)) prompts

      inputCheckbox (Tuple index prompt) = HH.label [ HP.for (toString index) ]
                                                    [ HH.input [ HP.type_ HP.InputCheckbox
                                                               , HP.id (toString index)
                                                               , HP.name "checked"
                                                               , HP.value prompt.prompt
                                                               , HP.checked false
                                                               , HEV.onChecked \b -> (Put prompt b)
                                                               ]
                                                    , HH.text prompt.prompt
                                                    ]
                                           where toString :: Int -> String
                                                 toString = Int.toStringAs Int.decimal

initialize :: forall m. MonadEffect m => H.HalogenM _ _ _ _ m Unit
initialize = undefined

-- checkbox
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
                   , HEV.onChecked action.handleChange
                   , HEV.onBlur action.handleBlur
                   ]
                 , HH.text label
                 ]
               ]
