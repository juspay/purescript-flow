{-
Copyright (c) 2012-2017 "JUSPAY Technologies"
JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]

This file is part of JUSPAY Platform.

JUSPAY Platform is free software: you can redistribute it and/or modify
it for only educational purposes under the terms of the GNU Affero General
Public License (GNU AGPL) as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.
For Enterprise/Commerical licenses, contact <info@juspay.in>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
be liable for all damages without limitation, which is caused by the
ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
damages, claims, cost, including reasonable attorney fee claimed on Juspay.
The end user has NO right to claim any indemnification based on its use
of Licensed Software. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
-}

module Api where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic (class Generic) as G
import Data.Generic.Rep (class Generic)

data Header = Header String String
data HttpVerb = GET | POST
type StatusCode = Int

newtype Request = Request {
  url :: String
  , method :: HttpVerb
  , headers :: Array Header
  , content :: String
}

newtype Response = Response {
  status :: StatusCode
  , headers :: Array Header
  , response :: String
}

class Requestable a where
  toRequest :: Encode a => a -> Request

class Responsdable b where
  fromResponse :: forall e. Decode b => Response -> Aff e b

class ApiRequest a b where
  request :: forall e. Requestable a => Responsdable b => a -> Aff e b


getEncodedRequest :: forall a. Requestable a => Encode a => a -> String
getEncodedRequest = encodeJSON <<< toRequest

defaultPost :: { method :: HttpVerb
               , url :: String
               , content :: String
               , headers :: Array Header }

defaultPost = { method: POST
              , url: ""
              , content: ""
              , headers: [Header "ContentType" "application/json"] }

isValidAction :: forall a e. Decode a => String -> Aff e a
isValidAction x = case (runExcept (decodeJSON x)) of
  Right y -> pure $ y
  Left err -> throwError (error (show err))


genericFromResponse :: forall e b. Decode b => Response -> Aff e b
genericFromResponse (Response {status: s, response : r, headers : h}) = case
  (runExcept (decodeJSON r)) of
    Right resp -> pure $ resp
    Left err -> throwError (error $ (show err) <> " response: " <> r)

derive instance genericHeader :: Generic Header _
derive instance gHeader :: G.Generic Header
instance encodeHeader :: Encode Header where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x
instance decodeHeader :: Decode Header where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericHttpVerb :: Generic HttpVerb _
instance encodeHttpVerb :: Encode HttpVerb where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericRequest :: Generic Request _
instance encodeRequest :: Encode Request where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericResponse :: Generic Response _
derive instance gResponse :: G.Generic Response
instance decodeResponse :: Decode Response where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x
instance encodeResponse :: Encode Response where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x
