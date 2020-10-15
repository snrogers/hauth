module Adapter.InMemory.Auth where

import ClassyPrelude
import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Has
import Data.Maybe
import Data.Tuple.Sequence
import Text.StringRandom

import qualified Domain.Auth.Types as D


-- ----------------------------------------------------------------- --
-- Data Types
-- ----------------------------------------------------------------- --
data State = State
  { stateAuths :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId D.UserId
  } deriving (Eq)

initialState :: State
initialState = State
  { stateAuths = []
  , stateUnverifiedEmails = mempty
  , stateVerifiedEmails = mempty
  , stateUserIdCounter = 0
  , stateNotifications = mempty
  , stateSessions = mempty
  }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)


-- ----------------------------------------------------------------- --
-- Helpers
-- ----------------------------------------------------------------- --
orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = return a


-- ----------------------------------------------------------------- --
-- AuthRepo Instance
-- ----------------------------------------------------------------- --
addAuth :: InMemory r m
        => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let auths = stateAuths state
        doesExist = any (\(_, a) -> (D.authEmail auth ) == (D.authEmail a)) auths

    when doesExist $ throwE D.RegistrationErrorEmailTaken

    let newUserId = stateUserIdCounter state + 1
        newAuths = (D.UserId newUserId, auth) : auths
        unverifieds = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode (D.authEmail auth) unverifieds
        newState = state
          { stateAuths = newAuths
          , stateUnverifiedEmails = newUnverifieds
          }
    lift $ writeTVar tvar newState
    return (D.UserId newUserId, vCode)

findEmailFromUserId :: InMemory r m
                    => D.UserId -> m (Maybe D.Email)
findEmailFromUserId userId = do
  tvar <- asks getter
  auths <- liftIO $ stateAuths <$> readTVarIO tvar
  return $ D.authEmail <$> snd <$> find (\(id, _) -> id == userId) auths

findUserByAuth :: InMemory r m
               => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth userAuth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let auths = stateAuths state
  let mayUserId = map fst . find((userAuth ==) . snd) $ auths
  return $ mayUserId <&>
    \userId ->
      let verifieds = stateVerifiedEmails state
          email = D.authEmail userAuth
          isVerified = elem email verifieds
       in (userId, isVerified)

setEmailAsVerified :: InMemory r m
                   => D.VerificationCode
                   -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        mayEmail = lookup vCode unverifieds
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let auths = stateAuths state
        mayUId = map fst . find ((email ==) . D.authEmail . snd) $ auths
    uId <- mayUId `orThrow` D.EmailVerificationErrorInvalidCode
    let verifieds = stateVerifiedEmails state
        newVerifieds = insertSet email verifieds
        newUnverifieds = deleteMap vCode unverifieds
        newState = state
          { stateUnverifiedEmails = newUnverifieds
          , stateVerifiedEmails = newVerifieds
          }
    lift $ writeTVar tvar newState
    return (uId, email)

setEmailAsVerifiedMine :: InMemory r m
                   => D.VerificationCode
                   -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerifiedMine vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        verifieds = stateVerifiedEmails state
        auths = stateAuths state
        mayEmail = lookup vCode unverifieds
    case mayEmail of
      Nothing -> throwE D.EmailVerificationErrorInvalidCode
      Just email -> do
        let newUnverifieds = deleteMap vCode unverifieds
            newVerifieds = insertSet email verifieds
            newState = state
              { stateUnverifiedEmails = newUnverifieds
              , stateVerifiedEmails = newVerifieds
              }
        lift $ writeTVar tvar newState
        let Just uId = fst <$> find (\(_, a) -> (D.authEmail a) == email) auths
        return (uId, email)


-- ----------------------------------------------------------------- --
-- EmailVerificationNotif Instance
-- ----------------------------------------------------------------- --
findUserBySessionId :: InMemory r m
                    => D.SessionId -> m (Maybe D.UserId)
findUserBySessionId sessionId = do
  tvar <- asks getter
  liftIO $ lookup sessionId . stateSessions <$> readTVarIO tvar

newSession :: InMemory r m
           => D.UserId -> m D.SessionId
newSession userId = do
  tvar <- asks getter
  sessionId <- liftIO
    $ D.SessionId . ((tshow userId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = insertMap sessionId userId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sessionId


-- ----------------------------------------------------------------- --
-- SessionRepo Instance
-- ----------------------------------------------------------------- --
notifyEmailVerification :: InMemory r m
                        => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState = state { stateNotifications = newNotifications }
    writeTVar tvar newState

-- FOR TESTING ONLY: ???
getNotificationsForEmail :: InMemory r m
                         => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let notifications = stateNotifications state
      vCode = lookup email notifications
  return vCode
