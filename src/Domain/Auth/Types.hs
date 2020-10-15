module Domain.Auth.Types
  ( -- * Types
    Auth (..)
  , Email (rawEmail)
  , mkEmail
  , Password (rawPassword)
  , mkPassword
  , UserId (..)
  , VerificationCode
  , SessionId (..)
  , RegistrationError (..)
  , EmailVerificationError (..)
  , LoginError (..)

    -- * Services
  , AuthService (..)
  ) where

import ClassyPrelude

import Domain.Validation
import Text.Regex.PCRE.Heavy


-- ----------------------------------------------------------------- --
-- Email
-- ----------------------------------------------------------------- --
newtype Email = Email { rawEmail :: Text }
    deriving (Eq, Ord, Show)

type EmailValidationErr = Text

mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail = validate Email
  [ regexMatches
    [re|^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$|]
    "Not a valid email"
  ]


-- ----------------------------------------------------------------- --
-- Password
-- ----------------------------------------------------------------- --
newtype Password = Password { rawPassword :: Text }
    deriving (Eq, Show)

data PasswordValidationErr
    = PasswordValidationErrLength Int
    | PasswordValidationErrMustContainUpperCase
    | PasswordValidationErrMustContainLowerCase
    | PasswordValidationErrMustContainNumber

mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate Password
    [ lengthBetween 5 50 "Should be between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]


-- ----------------------------------------------------------------- --
-- Auth
-- ----------------------------------------------------------------- --
data Auth = Auth
    { authEmail :: Email
    , authPassword :: Password
    } deriving (Eq)


-- ----------------------------------------------------------------- --
-- User/Session/Verification
-- ----------------------------------------------------------------- --
newtype UserId = UserId Int
  deriving (Eq, Read, Show)

type VerificationCode = Text

newtype SessionId = SessionId Text
  deriving (Eq, Ord, Read, Show)

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)

data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)

data RegistrationError
    = RegistrationErrorEmailTaken
    deriving (Show, Eq)


-- ----------------------------------------------------------------- --
-- AuthService
-- ----------------------------------------------------------------- --
class (Monad m) => AuthService m where
  getUser :: UserId -> m (Maybe Email)
  login :: Auth -> m (Either LoginError SessionId)
  register :: Auth -> m (Either RegistrationError ())
  resolveSessionId :: SessionId -> m (Maybe UserId)
  verifyEmail :: VerificationCode -> m (Either EmailVerificationError ())
