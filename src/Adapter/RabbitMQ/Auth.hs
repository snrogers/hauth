module Adapter.RabbitMQ.Auth where

import ClassyPrelude hiding (bracket, tryAny)

import Control.Exception.Safe
import Data.Aeson
import Data.Aeson.TH
import Network.AMQP
import Katip

import Adapter.RabbitMQ.Common
import Domain.Auth.Types


data EmailVerificationPayload = EmailVerificationPayload
  { emailVerificationPayloadEmail :: Text
  , emailVerificationPayloadVerificationCode :: Text
  }

class (Monad m) => EmailVerificationSender m where
  sendEmailVerification :: Email -> VerificationCode -> m ()

init :: (EmailVerificationSender m, KatipContext m, MonadCatch m)
     => State -> (m Bool -> IO Bool) -> IO ()
init state runner = do
  initQueue state "verifyEmail" "auth" "userRegistered"
  initConsumer state "verifyEmail" (consumeEmailVerification runner)

consumeEmailVerification :: (EmailVerificationSender m, KatipContext m, MonadCatch m)
                         => (m Bool -> IO Bool) -> Message -> IO Bool
consumeEmailVerification runner msg =
  runner $ consumeAndProcess msg handler
    where
      handler payload =
        case mkEmail (emailVerificationPayloadEmail payload) of
          Left err -> withMsgAndErr msg err $ do
            $(logTM) ErrorS "Email format is invalid. Rejecting."
            return False
          Right email -> do
            let vCode = emailVerificationPayloadVerificationCode payload
            sendEmailVerification email vCode
            return True

$(let structName = fromMaybe "" . lastMay . splitElem '.' . show
        $ ''EmailVerificationPayload
      lowercaseFirst (x:xs) = toLower [x] <> xs
      lowercaseFirst xs = xs
      options = defaultOptions
            { fieldLabelModifier = lowercaseFirst . drop (length structName)
            }
    in deriveJSON options ''EmailVerificationPayload)

notifyEmailVerification :: (Rabbit r m)
                        => Email -> VerificationCode -> m ()
notifyEmailVerification email vCode =
  publish "auth" "userRegistered" payload
    where payload = EmailVerificationPayload
            { emailVerificationPayloadEmail = rawEmail email
            , emailVerificationPayloadVerificationCode = vCode
            }

