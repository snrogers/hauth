module Lib
    ( libMain
    , runKatip
    ) where

import ClassyPrelude hiding (bracket)
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Trans.Except
import Katip
import Text.StringRandom


import qualified Config
import Domain.Auth.Service as D
import Domain.Auth.Types
import qualified Adapter.HTTP.Main as HTTP
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.Redis.Auth as Redis


libMain = main


type State = (PG.State, Redis.State, MQ.State, TVar M.State)
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving ( Applicative
             , Functor
             , Katip
             , KatipContext
             , Monad
             , MonadCatch
             , MonadFail
             , MonadIO
             , MonadReader State
             , MonadThrow
             )

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
  findUserBySessionId = Redis.findUserBySessionId
  newSession = Redis.newSession

instance AuthService App where
  getUser = D.getUser
  login = D.login
  register = D.register
  resolveSessionId = D.resolveSessionId
  verifyEmail = D.verifyEmail

instance MQAuth.EmailVerificationSender App where
  sendEmailVerification = MQAuth.notifyEmailVerification

run :: LogEnv -> State -> App a -> IO a
run le env
  = runKatipContextT le () mempty
  . (\inner -> inner `runReaderT` env)
  . unApp

main :: IO ()
main = do
  config <- Config.fromEnv
  mainWithConfig config

mainDev :: IO ()
mainDev = do
  let config = Config.devConfig
  mainWithConfig config

withState :: Config.Config -> (Int -> LogEnv -> State -> IO ()) -> IO ()
withState config action =
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState (Config.configPG config) $ \pgState ->
      Redis.withState (Config.configRedis config) $ \redisState ->
        MQ.withState (Config.configMQ config) $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action (Config.configPort config) le state

mainWithConfig :: Config.Config -> IO ()
mainWithConfig config =
  withState config $ \port le state@(_, _, mqState, _) -> do
    let runner = run le state
    MQAuth.init mqState runner
    HTTP.main port runner

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

