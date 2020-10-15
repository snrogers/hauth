module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Control.Monad.Except
import Data.ByteString.Char8 (readInt)
import Data.ByteString.Builder (intDec)
import Data.Has
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Katip
import Text.RawString.QQ
import Text.StringRandom

import Domain.Auth.Types as D


-- Needs MonadFail? MonadThrow?
type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadFail m, KatipContext m)

type State = Pool Connection

data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout :: NominalDiffTime
  }

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg action =
  bracket initPool cleanPool action
    where initPool = createPool openConn closeConn
                     (configStripeCount cfg)
                     (configIdleConnTimeout cfg)
                     (configMaxOpenConnPerStripe cfg)
          cleanPool = destroyAllResources
          openConn = connectPostgreSQL (configUrl cfg)
          closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action state

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> return ()
  where
    cmds = [ MigrationInitialization
           , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
           ]

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn


-- ----------------------------------------------------------------- --
-- PostgreSQL DataType Decoders
-- ----------------------------------------------------------------- --
instance FromField Email where
  fromField f mval = do
    let mayEmail = mval >>= \bstring ->
                              case mkEmail $ decodeUtf8 bstring of
                                Right email -> Just email
                                Left _ -> Nothing
    case mayEmail of
      Just email -> pure email
      Nothing -> returnError ConversionFailed f "Couldn't read user.email field"

instance FromField UserId where
  fromField field mayVal = do
    let mayUId = mayVal >>= (\bstring -> UserId <$> fst <$> readInt bstring)
    case mayUId of
      Just uId -> pure uId
      Nothing -> returnError ConversionFailed field "Couldn't read user.id field"

instance ToField Email where
  toField = Escape . encodeUtf8 . rawEmail

instance ToField Password where
  toField = Escape . encodeUtf8 . rawPassword

instance ToField UserId where
  toField (UserId uId) = Plain . intDec $ uId

-- ----------------------------------------------------------------- --
-- AuthRepo Implementation
-- ----------------------------------------------------------------- --
randomVCode :: IO VerificationCode
randomVCode = stringRandomIO "[A-Za-z0-9]{16}"

addAuth :: PG r m
        => Auth
        -> m (Either RegistrationError (UserId, VerificationCode))
addAuth (Auth email password) = do
  let emailRaw = rawEmail email
      passwordRaw = rawPassword password
  vCode <- liftIO $ ((tshow emailRaw <> "_") <>) <$> randomVCode
  result <- withConn $ \conn ->
    try $ query conn qry (emailRaw, passwordRaw, vCode)
  case result of
    Right [Only uId] -> return $ Right(uId, vCode)
    Right _ -> throwString "Should not happen: PG did not return userId"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
      if state == "32505" && "auths_email_key" `isInfixOf` msg
         then return $ Left RegistrationErrorEmailTaken
         else throwString $ "Unhandled PG exception:" <> show err
  where
    qry =
      [r| INSERT INTO auths
            (email, pass, email_verification_code, is_email_verified)
          VALUES
            (?, crypt(?, gen_salt('bf')), ?, 'f')
          RETURNING
            id
      |]


findEmailFromUserId :: PG r m
                    => UserId -> m (Maybe Email)
findEmailFromUserId uId = do
  [Only email] <- withConn $ \conn ->
    query conn qry (Only uId)
  return email
    where qry = [r| SELECT email
                    FROM auths
                    WHERE id = ?
                |]

findUserByAuth :: PG r m
               => Auth -> m (Maybe (UserId, Bool))
findUserByAuth auth = do
  let (Auth email password) = auth
      emailRaw = rawEmail email
      passwordRaw = rawPassword password
  result <- withConn $ \conn ->
    query conn qry (emailRaw, passwordRaw)
  return $ case result of
             [(uId, isVerified)] -> Just (uId, isVerified)
             _ -> Nothing
  where
    qry = [r| SELECT id, is_email_verified
              FROM auths
              WHERE email = ?
                AND pass = crypt(?, pass)
          |]

withUserIdContext :: KatipContext m => UserId -> m a -> m a
withUserIdContext uId = katipAddContext $ sl "userId" (show uId)

setEmailAsVerified :: PG r m
                   => VerificationCode
                   -> m (Either EmailVerificationError (UserId, Email))
setEmailAsVerified vCode = do
  result <- withConn $ \conn ->
    query conn qry (Only vCode)
  case result of
    [(uId, email)] -> do
      $(logTM) InfoS $ ls $ "uId: " <> (show uId) <> ", email: " <> (show email)
      case mkEmail email of
        Right email -> return $ Right(uId, email)
        _ -> throwString $
          "Should not happen: email in DB is not valid:" <> unpack email
    _ -> return $ Left EmailVerificationErrorInvalidCode
  where
    qry =
      [r| UPDATE auths
          SET is_email_verified = 't'
          WHERE email_verification_code = ?
          RETURNING id, cast (email as text)
      |]
