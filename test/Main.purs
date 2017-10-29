module Test.Main where

import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Console
import Network.MQTT
import Network.MQTT.Coroutine
import Prelude

import Control.Coroutine (Process, await, runProcess, ($$))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffLog
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)

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
main = launchAff_ do
  client <- liftEff $ connect url options
  log $ "Connected to " <> unwrap url

  let topic = Topic "test/purescript/mqtt"
  liftEff $ subscribe client topic
  log $ "Subscribed to " <> unwrap topic

  _ <- forkAff $ runProcess $ mqttProcess client
  log $ "Spawned coroutine process that logs events to stdout."

  let msg = Message "test"
  liftEff $ publish client topic msg
  liftEff $ publish client topic msg
  liftEff $ publish client topic msg
  log $ "Published " <> unwrap msg <> " to " <> unwrap topic

  delay $ Milliseconds 1000.0
  liftEff $ end client
  log $ "Disconnected from " <> unwrap url

mqttConsumer :: ∀ e. MQTTConsumer _ Unit
mqttConsumer = forever do
  e <- await
  case e of
    OnConnect -> do
      lift $ log "Connected to MQTT broker."
    OnMessage t m -> do
      lift
        $ log
        $ "Received message \"" <> unwrap m
        <> "\" on topic \""     <> unwrap t <> "\""
    OnClose -> do
      lift $ log "Connection closed."
