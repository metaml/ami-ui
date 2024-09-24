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

type Msg    = { content :: String, role :: String }
type MsgReq = { messages :: Array Msg, stream :: Boolean }
type MsgRes = { messages :: Array Msg, friend :: String }

talk :: MsgReq -> Aff MsgRes
talk req = do
  let url = "http://localhost:8000/talk"
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
  let url = "http://localhost:8000/prompts" -- @todo: change to ec2 URL
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
