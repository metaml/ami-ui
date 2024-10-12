module Ami where

import Prelude
import Data.Array
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Effect.Exception (Error)
import Fetch as F
import Foreign (unsafeFromForeign)
import Halogen as H
import Undefined (undefined)

type Msg    = { content :: String, role :: String, member :: String, friend :: String }
type MsgReq = { messages :: Array Msg, stream :: Boolean }
type MsgRes = { messages :: Array Msg, friend :: String }

baseUrl :: String
baseUrl = "https://alb-64c71258f6c9e59f.elb.us-east-2.amazonaws.com:8000"
--baseUrl = "https://localhost:8000"

talk :: MsgReq -> Aff MsgRes
talk req = do
  let url = baseUrl <> "/talk"
  res <- F.fetch url { method: F.POST
                     , body: stringify (encodeJson req)
                     , headers: { "Content-Type": "application/json" }
                     }
  json <- res.json
  let js = unsafeFromForeign json
      res' = decodeJson js :: Either _ MsgRes
  case res' of
    Left _  -> pure { messages: [], friend: "Courtney" }
    Right r -> pure r

type MessageReq = { member :: String, friend :: String }
type MessageRes = { name :: String, message :: String }

messages :: MessageReq -> Aff (Array MessageRes)
messages req = do
  let url = baseUrl <> "/messages"
  res <- F.fetch url { method: F.POST
                     , body: stringify (encodeJson req)
                     , headers: { "Content-Type": "application/json" }
                     }
  json <- res.json
  let js = unsafeFromForeign json
      res' = decodeJson js :: Either _ (Array MessageRes)
      r = case res' of
            Left _  -> []
            Right r -> r
  pure r

type PromptReq = { member :: String, friend :: String }
type Prompt = { prompt :: String, member :: String, friend :: String, enabled :: Boolean }
type Prompt' = { prompt :: String, member_id :: String, friend_id :: String, enabled :: Boolean }
type PromptRes = Array Prompt'

prompts :: PromptReq -> Aff (Array Prompt)
prompts req = do
  ps <- prompts' req
  let ps' = (\p -> { prompt: p.prompt, member: p.member_id, friend: p.friend_id, enabled: p.enabled }) <$> ps
  pure ps'

prompts' :: PromptReq -> Aff (Array Prompt')
prompts' req = do
  let url = baseUrl <> "/prompts"
  ps <- F.fetch url { method: F.POST
                    , body: stringify (encodeJson req)
                    , headers: { "Content-Type": "application/json" }
                    }
  psjson <- ps.json
  let json = unsafeFromForeign psjson
      res' = decodeJson json :: Either _ PromptRes
  H.liftEffect $ logShow res'
  case res' of
    Left e   -> (H.liftEffect $ logShow e)  *> pure []
    Right ps -> (H.liftEffect $ logShow ps) *> pure ps

promptsSystem :: PromptReq -> Aff (Array Prompt')
promptsSystem req = do
  let url = baseUrl <> "/prompts_system"
  ps <- F.fetch url { method: F.POST
                    , body: stringify (encodeJson req)
                    , headers: { "Content-Type": "application/json" }
                    }
  psjson <- ps.json
  let json = unsafeFromForeign psjson
      res' = decodeJson json :: Either _ PromptRes
  H.liftEffect $ logShow res'
  case res' of
    Left e   -> (H.liftEffect $ logShow e)  *> pure []
    Right ps -> (H.liftEffect $ logShow ps) *> pure ps

promptAdd :: Prompt' -> Aff Boolean
promptAdd p = do
  let url = baseUrl <> "/prompts/add"
  res <- F.fetch url { method: F.POST
                     , body: stringify (encodeJson p)
                     , headers: { "Content-Type": "application/json" }
                     }
  json <- res.json
  let js = unsafeFromForeign json
      b = decodeJson js :: Either _ Boolean
  case b of
    Left e  -> (H.liftEffect $ logShow e) *> pure false
    Right r -> pure r

promptUpdate :: Prompt' -> Aff Boolean
promptUpdate p = do
  let url = baseUrl <> "/prompts/update"
  res <- F.fetch url { method: F.POST
                     , body: stringify (encodeJson p)
                     , headers: { "Content-Type": "application/json" }
                     }
  json <- res.json
  let js = unsafeFromForeign json
      b = decodeJson js :: Either _ Boolean
  case b of
    Left e  -> (H.liftEffect $ logShow e) *> pure false
    Right r -> pure r
