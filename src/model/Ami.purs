module Ami where

import Prelude
import Control.Bind ((=<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (parseJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either
import Data.Functor
import Effect.Aff (Aff, attempt)
import Effect.Console (log, logShow)
import Fetch as F
import Fetch.Argonaut.Json (fromJson)
import Foreign
import Halogen as H
import Undefined

type Message  = { content :: String, role :: String }
type Request  = { messages :: Array Message, stream :: Boolean }
type Response = { messages :: Array Message, friend :: String }

talk :: Request -> Aff Response
talk req = do
  let url = "http://localhost:8000/talk"
  r <- F.fetch url { method: F.POST
                   , body: stringify (encodeJson req)
                   , headers: { "Content-Type": "application/json" }
                   }
  json <- r.json
  let js = (unsafeFromForeign json)
      res = decodeJson js :: Either _ Response
  case res of
    Left e  -> pure { messages: [], friend: "Courtney" }
    Right r -> pure r
