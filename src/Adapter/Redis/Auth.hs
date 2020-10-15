module Adapter.Redis.Auth where

import ClassyPrelude
import Data.Has
import Katip
import Text.StringRandom
import qualified Database.Redis as R
import qualified Domain.Auth.Types as D

type State = R.Connection

type Redis r m = (Has State r, KatipContext m, MonadReader r m, MonadIO m)

-- |Create state from redis url string.
-- format: redis://user:pass@host:port/db
-- sample: redis://user:pass@localhost:6379/0
withState :: String -> (State -> IO a) -> IO a
withState connUrl action = do
  case R.parseConnectInfo connUrl of
    Left _ -> throwString "Invalid Redis conn URL"
    Right connInfo -> do
      conn <- R.checkedConnect connInfo
      action conn

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action


-- ----------------------------------------------------------------- --
-- SessionRepo Implementation
-- ----------------------------------------------------------------- --
findUserBySessionId :: Redis r m
                    => D.SessionId
                    -> m (Maybe D.UserId)
findUserBySessionId sId = do
  result <- withConn $ R.get . encodeUtf8 . tshow $ sId
  case result of
    Right mayUId -> return $ readMay =<< unpack . decodeUtf8 <$> mayUId
    err -> throwString $ "Unexpected redis error: " <> show err

newSession :: Redis r m
           => D.UserId
           -> m D.SessionId
newSession uId = do
  sId <- liftIO $ D.SessionId . ((tshow uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  result <- withConn $ R.set (encodeUtf8 $ tshow sId) (fromString . show $ uId)
  case result of
    Left reply -> throwString $ "Unexpected redis error: " <> show reply
    Right R.Ok -> return sId
