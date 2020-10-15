module Adapter.HTTP.Main where

import ClassyPrelude

import Katip
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Vhost
import Web.Scotty.Trans

import Adapter.HTTP.API.Server.Common
import Adapter.HTTP.Common
import Domain.Auth.Types
import qualified Adapter.HTTP.API.Server.Main as API
import qualified Adapter.HTTP.Web.Main as Web


main :: ( AuthService m
        , KatipContext m
        )
     => Int -> (m Response -> IO Response) -> IO ()
main port runner = do
  web <- Web.main runner
  api <- API.main runner
  run port $ vhost [(pathBeginsWith "api", api)] web
    where
      pathBeginsWith path req = headMay (pathInfo req) == Just path

