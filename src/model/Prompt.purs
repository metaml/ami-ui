module Prompt where

import Prelude
-- import Ami as A
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
type Form f = ( prompt :: f String String String
              , member :: f String String String
              , friend :: f String String String
              )           -- input  error  output

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action = Receive FormContext | Eval FormlessAction
type FormInputs = { | Form F.FieldInput }

type Query :: forall k. k -> Type
type Query = Const Void -- forall k. k -> Type
type Input = Unit -- { | Form F.FieldInput }
type Output = { | Form F.FieldOutput }

type Prompt = { | Form F.FieldOutput }
type State = { context :: FormContext, prompts :: Array Prompt }

component :: _ _ _ Aff
component = Html.mkComponent "prompt" "" form

form :: forall query. H.Component query Input Output Aff
form = F.formless { liftAction: Eval } mempty
       $ H.mkComponent { initialState: \ctx -> { context: ctx, prompts: [] }
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
        H.modify_ _ { prompts = state'.prompts <> [ { prompt: fields.prompt
                                                    , member: fields.member
                                                    , friend: fields.friend
                                                    }
                                                  ]
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
  -- let lines = intersperse HH.br_ $ map (\{ member, prompt } -> HH.text (member <> ": " <> prompt)) prompts
  let container = HP.class_ (ClassName "container")
      pmfs = (\p -> HH.text (p.prompt <> " | " <> p.member <> " | " <>  p.friend)) <$> prompts
      pmfHtmls = (\p -> HH.article [ container ] [ p ]) <$> pmfs
  HH.form [ HEV.onSubmit formActions.handleSubmit ]
          [ HH.div_ [ HH.label_ []
                    , HH.ul_ pmfHtmls
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
