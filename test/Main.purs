module Test.Main where

import Prelude
import Network.MQTT

import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log) as AffLog
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)

options :: Options
options =
  { port : 1883
  , clientId : "purescript-mqtt"
  , username : mempty
  , password : mempty
  }

url :: BrokerUrl
url = wrap "mqtt://broker.hivemq.com"

main :: Eff _ _
main = void $ launchAff $ do
  client <- liftEff $ connect url options

  onConnectAff client $ do
    AffLog.log "Connected!"
    subscribe client $ Topic "purescript-mqtt/test"
    AffLog.log "Subscribed!"

  publish client (Topic "purescript-mqtt/test") (Message "test")

  msg <- onMessageAff client $ \_ msg ->
    pure $ unwrap msg

  AffLog.log msg

  liftEff $ end client
