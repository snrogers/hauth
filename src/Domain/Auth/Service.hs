module Domain.Auth.Service where

import ClassyPrelude

import Control.Monad.Except
import Katip

import Domain.Auth.Types


-- ----------------------------------------------------------------- --
-- Contexts
-- ----------------------------------------------------------------- --
class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  findEmailFromUserId :: UserId -> m (Maybe Email)
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  setEmailAsVerified :: VerificationCode
                     -> m  (Either EmailVerificationError (UserId, Email))

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  findUserBySessionId :: SessionId -> m (Maybe UserId)
  newSession :: UserId -> m SessionId


-- ----------------------------------------------------------------- --
-- ????
-- ----------------------------------------------------------------- --
withUserIdContext :: KatipContext m => UserId -> m a -> m a
withUserIdContext uId = katipAddContext $ sl "userId" (show uId)

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

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
