module Adapter.HTTP.Common where

import ClassyPrelude

import Blaze.ByteString.Builder (toLazyByteString)
import Data.Aeson hiding (json)
import Data.Time.Lens
import Domain.Auth
import Katip
import Network.HTTP.Types.Status
import Web.Scotty.Trans
import qualified Text.Digestive.Aeson as DF
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
-- import qualified Text.Digestive as DF
import Web.Cookie

import Domain.Auth


-- ----------------------------------------------------------------- --
-- JSON
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

toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success


-- ----------------------------------------------------------------- --
-- Cookies
-- ----------------------------------------------------------------- --
setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie =
  setHeader "Set-Cookie"
  . decodeUtf8
  . toLazyByteString
  . renderSetCookie

getCookie :: (ScottyError e, Monad m)
          => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val

setSessionIdInCookie :: (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $ def { setCookieName = "sId"
                  , setCookiePath = Just "/"
                  , setCookieValue = encodeUtf8 . tshow $ sId
                  , setCookieExpires = Just $ modL month (+ 1) curTime
                  , setCookieHttpOnly = True
                  , setCookieSecure = False
                  , setCookieSameSite = Just sameSiteLax
                  }

getCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  maySId <- getCookie "sId" <&> (<&> SessionId)
  -- let mayUId = maySId >>= resolveSessionId
  case maySId of
    Nothing -> return Nothing
    Just sId -> lift $ resolveSessionId sId

reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUId <- getCurrentUserId
  case mayUId of
    Nothing -> do
      status status401
      json $ tshow "AuthRequired"
      finish
    Just uId -> return uId

