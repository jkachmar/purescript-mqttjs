module Test.Main where

import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Console
import Control.Monad.Eff.Console as EffLog
import Network.MQTT
import Prelude

import Control.Coroutine (Process, await, runProcess, ($$))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (mempty)
import Data.Newtype (wrap)

url :: BrokerUrl
url = wrap "mqtt://broker.hivemq.com"

options :: Options
options =
  { port     : 1883
  , clientId : "purescript-mqtt-test"
  , username : mempty
  , password : mempty
  }

mqttProcess :: ∀ e. Client -> Process (Aff _) Unit
mqttProcess client =
  let mqttProducer = mkMQTTProducer client
  in mqttProducer $$ mqttConsumer

main :: Eff _ _
main = do
  client <- connect url options
  EffLog.log "Started client."

  subscribe client $ Topic "test/purescript/mqtt"
  EffLog.log "Subscribed to \"test/purescript/mqtt\""

  launchAff_ $ runProcess $ mqttProcess client

  publish client (Topic "test/purescript/mqtt") (Message "test")

  launchAff_ $ do
    delay $ Milliseconds 1000.0
    liftEff $ end client

mqttConsumer :: ∀ e. MQTTConsumer _ Unit
mqttConsumer = forever do
  e <- await
  case e of
    OnConnect ->
      lift $ log "Connected"
    OnMessage (Topic t) (Message m) -> do
      lift $ log $ "Received new message on topic: " <> t
      lift $ log m
    OnClose ->
      lift $ log "Connection closed."
