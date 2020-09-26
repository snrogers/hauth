module Adapter.HTTP.Main where

import ClassyPrelude hiding(delete)

import Katip
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans

import qualified Adapter.HTTP.API.Auth as AuthAPI
import Adapter.HTTP.Common
import Domain.Auth


main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)
     => Int -> (m Response -> IO Response) -> IO ()
main port runner =
  scottyT port runner routes

routes :: ( AuthRepo m
          , EmailVerificationNotif m
          , KatipContext m
          , MonadIO m
          , SessionRepo m
          )
       => ScottyT LText m ()
routes = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware $ logStdoutDev

  AuthAPI.routes

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error:" <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)
