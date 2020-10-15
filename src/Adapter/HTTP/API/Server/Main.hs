module Adapter.HTTP.API.Server.Main where

import ClassyPrelude

import Katip
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans

import qualified Adapter.HTTP.API.Server.Auth as Auth
import Adapter.HTTP.API.Server.Common
import Adapter.HTTP.Common
import Domain.Auth.Types


main :: (AuthService m, KatipContext m)
     => (m Response -> IO Response) -> IO Application
main runner =
  scottyAppT runner routes

routes :: (AuthService m, KatipContext m)
       => ScottyT LText m ()
routes = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware $ logStdoutDev

  Auth.routes

  notFound $ do
    status status404
    json $ errorResponse ("NotFound" :: Text)

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error:" <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)

