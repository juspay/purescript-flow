
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

module ApiExample where

import Prelude
import Api (class ApiRequest, class Requestable, class Responsdable, Request(Request)
            , Response, defaultPost, fromResponse, genericFromResponse, isValidAction, request)
import Control.Monad.Aff (Aff, makeAff)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)

baseUrl = "http://juspay.api" :: String

newtype RequestOtp = RequestOtp {mobileNumber :: String}
data RequestOtpResp = RequestOtpResp {status :: String} | FailureResp {error :: String, message :: String}

requestOtpFlow :: forall e. Aff e String
requestOtpFlow = do
  res <- request (RequestOtp {mobileNumber: "919999999999"})
  case res of
    RequestOtpResp {status: s} -> pure $ "Verify Otp"
    FailureResp {message: msg} -> pure $ "Error " <> msg


instance requestOtp :: Requestable RequestOtp where
  toRequest x = Request defaultPost {url = baseUrl <> "/api/v1/otp", content=(encodeJSON x)}

instance requestOtpResp :: Responsdable RequestOtpResp where
  fromResponse = genericFromResponse

instance requestOtpReq :: ApiRequest RequestOtp RequestOtpResp where
  request x = genericRequestHandler x >>= fromResponse


genericRequestHandler :: forall a e. Requestable a => Encode a => a -> Aff e Response
genericRequestHandler x = (makeAff (\err sc -> sc (encodeJSON (RequestOtpResp {status: "ok"})))) >>= isValidAction

derive instance genericRequestOtp :: Generic RequestOtp _
instance encodeRequestOtp :: Encode RequestOtp where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericRequestOtpResp :: Generic RequestOtpResp _
instance encodeRequestOtpResp :: Encode RequestOtpResp where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x
instance decodeRequestOtpResp :: Decode RequestOtpResp where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x
