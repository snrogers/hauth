module Lib
    ( someFunc
    , runKatip
    ) where

import ClassyPrelude
import Control.Monad.Trans.Except
import Control.Monad.Fail
import qualified Adapter.InMemory.Auth as M
import Domain.Auth
import Katip

type State = TVar M.State
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving ( Applicative
             , Functor
             , KatipContext
             , Katip
             , Monad
             , MonadReader State
             , MonadFail
             , MonadIO)

run :: LogEnv -> State -> App a -> IO a
run le state
  = runKatipContextT le () mempty
  . (\inner -> runReaderT inner state)
  . unApp

instance AuthRepo App where
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  findUserBySessionId = M.findUserBySessionId
  newSession = M.newSession


someFunc :: IO ()
someFunc  = withKatip $ \le -> do
  state <- newTVarIO M.initialState
  run le state action

action :: App ()
action = do
  let email = either undefined id $ mkEmail "ecky@test.com"
      password = either undefined id $ mkPassword "FakePass1"
      auth = Auth email password
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session <- login auth
  Just userId <- resolveSessionId session
  Just registeredEmail <- getUser userId
  print (session, userId, registeredEmail)


-- ----------------------------------------------------------------- --
-- DELETE AFTER THIS
-- ----------------------------------------------------------------- --
runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
    where
      createLogEnv = do
        logEnv <- initLogEnv "HAuth" "dev"
        -- stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (\_ -> return True) V2 -- log everything
        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2 -- log everything
        registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $ do
    $(logTM) InfoS "Log in ns1"
    katipAddNamespace "ns2" $ do
      $(logTM) WarningS "Log in ns2"
      katipAddNamespace "ns3" $
        katipAddContext (sl "userId" $ asText "12") $ do
          $(logTM) InfoS "Log in ns2.ns3 with userId context"
          katipAddContext (sl "country" $ asText "Singapore") $
            $(logTM) InfoS "Log in ns2.ns3 with userId and country contexts"

