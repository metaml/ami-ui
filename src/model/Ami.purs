module Ami where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Fetch as F
import Foreign (unsafeFromForeign)

type Message  = { content :: String, role :: String }
type Request  = { messages :: Array Message, stream :: Boolean }
type Response = { messages :: Array Message, friend :: String }

talk :: Request -> Aff Response
talk req = do
  let url = "http://localhost:8000/talk"
  res <- F.fetch url { method: F.POST
                     , body: stringify (encodeJson req)
                     , headers: { "Content-Type": "application/json" }
                     }
  json <- res.json
  let js = unsafeFromForeign json
      res' = decodeJson js :: Either _ Response
  case res' of
    Left _  -> pure { messages: [], friend: "Courtney" }
    Right r -> pure r
