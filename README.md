# purescript-mqttjs

[![Build Status](https://travis-ci.org/jkachmar/purescript-mqttjs.svg?branch=master)](https://travis-ci.org/jkachmar/purescript-mqttjs)

A PureScript wrapper around [MQTT.js](https://github.com/mqttjs/MQTT.js).

## Example Usage

```purs
import Prelude
import Network.MQTT
import Network.MQTT.Coroutine

import Control.Coroutine (Process, ($$), await, runProcess)
import Control.Monad.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)

-- MQTT broker address.
brokerUrl :: BrokerUrl
brokerUrl = BrokerUrl "mqtt://broker.hivemq.com"

-- MQTT client connection options.
connOptions :: Options
connOptions =
  { port     : 1883
  , clientId : "purescript-mqttjs-example"
  , username : mempty
  , password : mempty
  }

-- Topic to subscribe and/or publish to on the MQTT broker.
topic :: Topic
topic = Topic "test/purescript/mqttjs"

-- Message to publish to a topic on the MQTT broker.
message :: Message
message = Message "example MQTT message"

-- Creates a coroutine consumer that prints MQTT events and contents.
mqttConsumer :: MQTTConsumer _ Unit
mqttConsumer = forever do
  e <- await
  case e of
    OnConnect -> lift $ log "Client connected!"
    OnClose   -> lift $ log "Client disconnected!"
    OnMessage t m -> lift $ do
      log $ "Received a message from a subscription."
      log $ "Topic: \"" <> unwrap t <> "\" \t-\t Message: \"" <> unwrap m <> "\""

-- Creates coroutine process that prints all MQTT events from a given client to
-- the console.
mqttProcess :: Client -> Process (Aff _) Unit
mqttProcess client =
  let mqttProducer = mkMQTTProducer client
  in  mqttProducer $$ mqttConsumer

main :: Eff _ Unit
main = launchAff_ $ do
  client <- liftEff $ connect brokerUrl connOptions
  _ <- forkAff $ runProcess $ mqttProcess client
  
  liftEff $ subscribe client topic
  liftEff $ publish client topic message
  
  delay $ Milliseconds 1000.0
  liftEff $ end client
```
