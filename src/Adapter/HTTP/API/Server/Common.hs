module Adapter.HTTP.API.Server.Common where

import ClassyPrelude

import Blaze.ByteString.Builder (toLazyByteString)
import Data.Aeson ((.=), Value(Null), ToJSON, object)
import Katip
import Network.HTTP.Types.Status
import Web.Scotty.Trans
import qualified Text.Digestive.Aeson as DF
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF

import Adapter.HTTP.Common
import Domain.Auth


-- ----------------------------------------------------------------- --
-- JSON (Forms)
-- ----------------------------------------------------------------- --
parseAndValidateJSON :: (KatipContext m, ScottyError e, MonadIO m, ToJSON v)
                     => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val <- rescue jsonData (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) ->
      return result


-- ----------------------------------------------------------------- --
-- Cookies (Sessions)
-- ----------------------------------------------------------------- --
reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUId <- getCurrentUserId
  case mayUId of
    Nothing -> do
      status status401
      json $ tshow "AuthRequired"
      finish
    Just uId -> return uId

-- * Error Response
errorResponse :: (ToJSON a) => a -> Value
errorResponse val = object [ "error" .= val ]
