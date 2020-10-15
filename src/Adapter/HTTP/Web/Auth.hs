module Adapter.HTTP.Web.Auth where

import ClassyPrelude

import Web.Scotty.Trans
import Text.Digestive.Scotty
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import Text.Digestive.Form ((.:))
import Katip
import Text.Blaze.Html5 ((!))
import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.Auth.Types


-- * Routes
routes :: ( ScottyError e
          , MonadIO m
          , KatipContext m
          , AuthService m
          )
       => ScottyT e m ()
routes = do
  -- home
  get "/" $
    redirect "/users"

  -- register
  get "/auth/register" getAuthRegisterHandler
  post "/auth/register" postAuthRegisterHandler

  -- verify email
  get "/auth/verifyEmail/:code" getVerifyEmailHandler

  -- login
  get "/auth/login" getAuthLoginHandler
  post "/auth/login" postAuthLoginHandler

  -- get user
  get "/users" getUsersHandler

-- ----------------------------------------------------------------- --
-- ----------------------------------------------------------------- --
getUsersHandler :: (ScottyError e, KatipContext m, AuthService m)
                => ActionT e m ()
getUsersHandler = do
  userId <- reqCurrentUserId
  mayEmail <- lift $ getUser userId
  case mayEmail of
    Nothing ->
      raise $ stringError "Should not happen: email is not found"
    Just email ->
      renderHtml $ usersPage (rawEmail email)
  where
    usersPage :: Text -> H.Html
    usersPage email =
      mainLayout "Users" $ do
        H.div $
          H.h1 "Users"
        H.div $
          H.toHtml email

getVerifyEmailHandler :: (ScottyError e, AuthService m, KatipContext m)
                      => ActionT e m ()
getVerifyEmailHandler = do
  vCode <- param "code" `rescue` const (return "")
  verifResultEither <- lift $ verifyEmail vCode
  case verifResultEither of
    Left emailVerifError ->
      renderHtml $ verifyEmailPage "The verification code is invalid"
    Right _ ->
      renderHtml $ verifyEmailPage " Your Email has been verified"
  where
    verifyEmailPage :: Text ->H.Html
    verifyEmailPage msg =
      mainLayout "EmailVerification" $ do
        H.h1 "EmailVerification"
        H.div $ H.toHtml msg
        H.div $ H.a ! A.href "/auth/login" $ "Login"

getAuthLoginHandler :: (ScottyError e, Monad m) => ActionT e m ()
getAuthLoginHandler = do
  view <- DF.getForm "auth" authForm
  renderHtml $ loginPage view []

postAuthLoginHandler :: (ScottyError e, AuthService m, KatipContext m)
                     => ActionT e m ()
postAuthLoginHandler = do
  (view, mayAuth) <- runForm "auth" authForm
  case mayAuth of
    Nothing ->
      renderHtml $ loginPage view []
    Just auth -> do
      result <- lift $ login auth
      case result of
        Left LoginErrorEmailNotVerified ->
          renderHtml $ loginPage view ["Email has not been verified"]
        Left LoginErrorInvalidAuth ->
          renderHtml $ loginPage view ["Email/password is incorrect"]
        Right sId -> do
          setSessionIdInCookie sId
          redirect "/"

getAuthRegisterHandler :: (ScottyError e, Monad m) => ActionT e m ()
getAuthRegisterHandler = do
  view <- DF.getForm "auth" authForm
  renderHtml $ registerPage view []

postAuthRegisterHandler :: (ScottyError e, KatipContext m, AuthService m)
                        => ActionT e m ()
postAuthRegisterHandler = do
  (view, mayAuth) <- runForm "auth" authForm
  case mayAuth of
    Nothing ->
      renderHtml $ registerPage view []
    Just auth -> do
      result <- lift $ register auth
      case result of
        Left RegistrationErrorEmailTaken ->
          renderHtml $ registerPage view ["Email has been taken"]
        Right _ -> do
          v <- DF.getForm "auth" authForm
          renderHtml $ registerPage v ["Registered successfully"]


-- ----------------------------------------------------------------- --
-- ----------------------------------------------------------------- --
authForm :: (Monad m) => DF.Form [Text] m Auth
authForm =
  Auth <$> "email" .: emailForm
  <*>  "password" .: passwordForm
  where
    emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

authFormLayout :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
authFormLayout view formTitle action msgs =
  formLayout view action $ do
    H.h2 $ H.toHtml formTitle
    H.div $ do
      H.label "Email"
      DH.inputText "email" view
      H.div $
        errorList' "email"
    H.div $ do
      H.label "Password"
      DH.inputPassword "password" view
      H.div $
        errorList' "password"
    H.input ! A.type_ "submit" ! A.value "Submit"
  where
    errorList' path =
      errorList . mconcat $ DF.errors path view
    errorList =
      H.ul . concatMap errorItem
    errorItem =
      H.li . H.toHtml

loginPage :: DF.View [Text] -> [Text] -> H.Html
loginPage view msgs =
  mainLayout "Login" $ do
    H.div $
      authFormLayout view "Login" "/auth/login" msgs
    H.div $
      H.a ! A.href "/auth/register" $ "Register"

registerPage :: DF.View [Text] -> [Text] -> H.Html
registerPage view msgs =
  mainLayout "Register" $ do
    H.div $
      authFormLayout view "Register" "/auth/register" msgs
    H.div $
      H.a ! A.href "/auth/login" $ "Login"
