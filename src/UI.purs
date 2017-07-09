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

module UI where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Generic.Rep (class Generic)

foreign import data UI :: Effect

class UIScreen a b where
  ui::forall e. Encode b => a -> Aff (ui::UI|e) b
  generateMockEvents :: Encode b => a -> Array b

isValidAction :: forall a e. Decode a => String -> Aff e a
isValidAction x = case (runExcept (decodeJSON x)) of
  Right y -> pure $ y
  Left err -> throwError (error (show err))

defaultDecode :: forall a b. Generic a b => GenericDecode b => Foreign -> F a
defaultDecode x = genericDecode (defaultOptions {unwrapSingleConstructors=true}) x

defaultEncode ::  forall a b. Generic a b => GenericEncode b => a -> Foreign
defaultEncode x = genericEncode (defaultOptions {unwrapSingleConstructors=true}) x
