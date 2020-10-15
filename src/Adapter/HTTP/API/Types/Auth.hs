module Adapter.HTTP.API.Types.Auth where

import ClassyPrelude

import Data.Aeson

import Adapter.HTTP.API.Types.AesonHelper
import Domain.Auth.Types


instance FromJSON Email where
  parseJSON =
    withText "Email" $ withSmartConstructor mkEmail

instance FromJSON Password where
  parseJSON =
    withText "Password" $ withSmartConstructor mkPassword

$(map concat . sequence $
  [ deriveJSONRecord ''Auth
  , deriveToJSONUnwrap ''Email
  , deriveToJSONUnwrap ''Password
  , deriveJSONSumType ''RegistrationError
  , deriveJSONSumType ''EmailVerificationError
  , deriveJSONSumType ''LoginError
  ])





