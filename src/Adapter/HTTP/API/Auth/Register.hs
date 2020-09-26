module Adapter.HTTP.API.Auth.Register where

import ClassyPrelude

import Data.Aeson hiding (json, (.:))
import Katip
import Network.HTTP.Types.Status
import Text.Digestive.Form ((.:))
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF

import Adapter.HTTP.Common
import Domain.Auth


