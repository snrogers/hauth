module Adapter.HTTP.Web.Main where

import ClassyPrelude

import Katip
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Gzip
import Web.Scotty.Trans

import qualified Adapter.HTTP.Web.Auth as Auth
import Domain.Auth


main :: ( MonadIO m
        , KatipContext m
        , AuthRepo m
        , EmailVerificationNotif m
        , SessionRepo m
        )
     => (m Response -> IO Response) -> IO Application
main runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT runner $ routes cacheContainer

routes :: ( MonadIO m
          , KatipContext m
          , AuthRepo m
          , EmailVerificationNotif m
          , SessionRepo m
          )
       => CacheContainer -> ScottyT LText m ()
routes cachingStrategy = do
  middleware $
    gzip $ def { gzipFiles = GzipCompress }
  middleware $
    staticPolicy' cachingStrategy (addBase "src/Adapter/HTTP/Web/static")

  Auth.routes

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error:" <> ls (showError e)
    status status500
    text "Internal server error!"
