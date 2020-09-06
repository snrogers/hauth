module Lib
    ( main
    , runKatip
    ) where

import ClassyPrelude hiding (bracket)
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Trans.Except
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.Redis.Auth as Redis
import Domain.Auth
import Katip
import Text.StringRandom

type State = (PG.State, Redis.State, MQ.State, TVar M.State)
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving ( Applicative
             , Functor
             , KatipContext
             , Katip
             , Monad
             , MonadCatch
             , MonadThrow
             , MonadFail
             , MonadIO
             , MonadReader State
             )

run :: LogEnv -> State -> App a -> IO a
run le state
  = runKatipContextT le () mempty
  . (\inner -> runReaderT inner state)
  . unApp

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


main :: IO ()
main =
  withState $ \le state@(_, _, mqState, _) -> do
    let runner = run le state
    MQAuth.init mqState runner
    runner action

withState :: (LogEnv -> State -> IO ()) -> IO ()
withState action =
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action le state
  where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"
    pgCfg = PG.Config
      { PG.configUrl = "postgresql://localhost/hauth"
      , PG.configStripeCount = 2
      , PG.configMaxOpenConnPerStripe = 5
      , PG.configIdleConnTimeout = 10
      }
    redisCfg = "redis://localhost:6379/0"


action :: App ()
action = do
  randEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@test\\.com"
  let email = either undefined id $ mkEmail randEmail
      password = either undefined id $ mkPassword "FakePass1"
      auth = Auth email password
  register auth
  vCode <- pollNotif email
  verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
  where
    pollNotif email = do
      result <- M.getNotificationsForEmail email
      case result of
        Nothing -> pollNotif email
        Just vCode -> return vCode


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

