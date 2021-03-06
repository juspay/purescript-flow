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

module UIExample where

import Prelude
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (encodeJSON)
import Data.Generic.Rep (class Generic)
import Flow.UI (class UIScreen, UI, defaultDecode, defaultEncode, ui, decodeAction)


data LoginScreen = LoginScreen String
data LoginScreenAction = RequestOtp String | Abort

instance loginScreen :: UIScreen LoginScreen LoginScreenAction where
  ui x = genericShowUI x

derive instance genericLoginScreenAction  :: Generic LoginScreenAction _
instance decodeLoginScreenAction :: Decode LoginScreenAction where decode = defaultDecode
instance encodeLoginScreenAction :: Encode LoginScreenAction where encode = defaultEncode

genericShowUI :: forall a b e. Encode b => Decode b => a -> Aff (ui::UI | e) b
genericShowUI a = do
  res <- makeAff (\err sc -> sc (encodeJSON (RequestOtp "919999999999")))
  decodeAction res

appLoginFlow :: Aff(ui :: UI) String
appLoginFlow = do
  action <- ui $ LoginScreen "Mary Jane"
  case action of
    RequestOtp mobileNumber -> pure $ "User requested otp"
    Abort -> pure $ "User aborted"

main :: Eff (exception :: EXCEPTION, ui :: UI) Unit
main = void $ launchAff $ appLoginFlow
