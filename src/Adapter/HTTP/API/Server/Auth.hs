module Adapter.HTTP.API.Server.Auth where

import ClassyPrelude

import Data.Aeson hiding (json, (.:))
import Katip
import Network.HTTP.Types.Status
import Text.Digestive.Form ((.:))
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF

import Adapter.HTTP.API.Server.Common
import Adapter.HTTP.API.Types.Auth
import Adapter.HTTP.Common
import Domain.Auth


routes :: ( ScottyError e
          , AuthRepo m
          , EmailVerificationNotif m
          , KatipContext m
          , MonadIO m
          , SessionRepo m
          )
       => ScottyT e m ()
routes = do
  post "/api/auth/register" $ registerHandler
  post "/api/auth/verifyEmail" $ verifyEmailHandler
  post "/api/auth/login" $ loginHandler

  get "/api/users" $ getUserHandler

  notFound $ text "404"


-- ----------------------------------------------------------------- --
-- Handler Definitions
-- ----------------------------------------------------------------- --
  -- Registration
  -- Req:
  -- { "email": _
  -- , "password": _
  -- }
  -- Res:
  -- - 400 { "email": "errmsg", "password": "errmgs" }
  -- - 400 "EmailTaken"
  -- - 200
registerHandler ::(AuthRepo m, EmailVerificationNotif m, KatipContext m, MonadIO m, ScottyError e)
                => ActionT e m ()
registerHandler = do
  input <- parseAndValidateJSON authForm
  domainResult <- lift $ register input
  case domainResult of
         Left err -> do
           status status400
           json err
         Right _ ->
           return ()

  -- Verification
  -- Req:
  -- { "email": _
  -- , "vCode": _
  -- }
  -- Res:
  -- - 400 "required
  -- - 400 "InvalidCode"
  -- - 200
verifyEmailHandler ::(AuthRepo m, KatipContext m, MonadIO m, ScottyError e)
                => ActionT e m ()
verifyEmailHandler = do
  input <- parseAndValidateJSON verificationCodeForm
  domainResult <- lift $ setEmailAsVerified input
  case domainResult of
    Left err -> do
      status status400
      json err
    Right _ ->
      return ()

  -- Login
  -- Req:
  -- { "email": _
  -- , "password": _
  -- }
  -- Res:
  -- - 400 Invalid Input: { "email": "errmsg", "password": "errmgs" }
  -- - 400 Invalid Auth: InvalidAuth
  -- - 400 Email not yet verified: "EmailNotVerified"
  -- - 200
loginHandler ::(AuthRepo m, KatipContext m, MonadIO m, ScottyError e, SessionRepo m)
                => ActionT e m ()
loginHandler = do
  input <- parseAndValidateJSON authForm
  domainResult <- lift $ login input
  case domainResult of
    Left err -> do
      status status400
      json err
    Right sId -> do
      setSessionIdInCookie sId
      return ()

  -- Get User
  -- Req:
  -- { "email": _
  -- , "password": _
  -- }
  -- Res:
  -- - 401 NotAuthenticated: "AuthRequired"
  -- - 200
getUserHandler :: (AuthRepo m, KatipContext m, MonadIO m, ScottyError e, SessionRepo m)
                 => ActionT e m ()
getUserHandler = do
  userId <- reqCurrentUserId
  mayEmail <- lift $ getUser userId
  case mayEmail of
    Nothing -> do
      raise $ stringError "Should not happen: SessionId map to invalid UserId"
    Just email ->
      json email




-- ----------------------------------------------------------------- --
-- Helpers
-- ----------------------------------------------------------------- --
authForm :: (Monad m) => DF.Form [Text] m Auth
authForm =
  Auth <$> "email" .: emailForm
       <*> "password" .: passwordForm
  where
    emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

verificationCodeForm :: (Monad m) => DF.Form [Text] m VerificationCode
verificationCodeForm = DF.text Nothing
