module Domain.Auth
  ( -- * Types
    Auth(..)
  , Email
  , EmailVerificationError(..)
  , LoginError(..)
  , Password
  , RegistrationError(..)
  , SessionId(..)
  , UserId(..)
  , VerificationCode
  , randomVCode
  , mkEmail
  , mkPassword
  , rawEmail
  , rawPassword

    -- * Ports
  , AuthRepo(..)
  , EmailVerificationNotif(..)
  , SessionRepo(..)

    -- * Use Cases
  , getUser
  , login
  , register
  , resolveSessionId
  , verifyEmail
  ) where

import ClassyPrelude

import Control.Monad.Except
import Domain.Validation
import Katip
import Text.Regex.PCRE.Heavy
import Text.StringRandom


data Auth = Auth
    { authEmail :: Email
    , authPassword :: Password
    } deriving (Eq)

data RegistrationError
    = RegistrationErrorEmailTaken
    deriving (Show, Eq)


-- ----------------------------------------------------------------- --
-- Helpers
-- ----------------------------------------------------------------- --
withUserIdContext :: KatipContext m => UserId -> m a -> m a
withUserIdContext uId = katipAddContext $ sl "userId" (show uId)

randomVCode :: IO VerificationCode
randomVCode = stringRandomIO "[A-Za-z0-9]{16}"


-- ----------------------------------------------------------------- --
-- Email
-- ----------------------------------------------------------------- --
newtype Email = Email { emailRaw :: Text }
    deriving (Eq, Ord, Show)

-- data EmailValidationErr = EmailValidationErrInvalidEmail
type EmailValidationErr = Text

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail = validate Email
  [ regexMatches
    [re|^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$|]
    "Not a valid email"
  ]


-- ----------------------------------------------------------------- --
-- Password
-- ----------------------------------------------------------------- --
newtype Password = Password {passwordRaw :: Text }
    deriving (Show, Eq)

data PasswordValidationErr
    = PasswordValidationErrLength Int
    | PasswordValidationErrMustContainUpperCase
    | PasswordValidationErrMustContainLowerCase
    | PasswordValidationErrMustContainNumber

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate Password
    [ lengthBetween 5 50 "Should be between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]


-- ----------------------------------------------------------------- --
-- VerificationCode
-- ----------------------------------------------------------------- --
type VerificationCode = Text
data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  findEmailFromUserId :: UserId -> m (Maybe Email)
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  setEmailAsVerified :: VerificationCode
                     -> m  (Either EmailVerificationError (UserId, Email))

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()


register :: (AuthRepo m, EmailVerificationNotif m, KatipContext m)
         => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls ("User registered" <> rawEmail email)

verifyEmail :: (KatipContext m, AuthRepo m)
            => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls ("User Email Verified: " <> rawEmail email)
  return ()


-- ----------------------------------------------------------------- --
-- User/Session
-- ----------------------------------------------------------------- --
newtype UserId = UserId Int
  deriving (Eq, Read, Show)
newtype SessionId = SessionId Text
  deriving (Eq, Ord, Read, Show)
data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)

class Monad m => SessionRepo m where
  findUserBySessionId :: SessionId -> m (Maybe UserId)
  newSession :: UserId -> m SessionId

login :: (AuthRepo m, KatipContext m, SessionRepo m)
      => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> do
      sId <- lift $ newSession uId
      withUserIdContext uId $
        $(logTM) InfoS $ ls ("User logged in: " <> show uId)
      return sId


-- ----------------------------------------------------------------- --
-- Public API
-- ----------------------------------------------------------------- --
getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId
