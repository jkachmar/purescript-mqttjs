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
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run')

--------------------------------------------------------------------------------
main :: Eff _ Unit
main =
  -- Set a longer timeout for testing against HiveMQ
  run' { slow: 1000, timeout: Just 10000 } [consoleReporter] do
  describe "purescript-mqttjs" do
    describe "Connection" do
      it "connects to and disconnects from the MQTT broker successfully" do
        -- Connect to the MQTT broker
        client <- liftEff $ connect url options

        -- Get an AVar and pass it off to the fiber running our MQTT process
        ref <- makeEmptyVar
        fiber <- forkAff $ runProcess $ mqttProcess client ref

        -- Disconnect from the MQTT broker
        liftEff $ end client

        -- Get the final value from the MQTT process and verify that no
        -- exceptions were thrown.
        res <- try $ joinFiber fiber
        isLeft res `shouldEqual` false

    describe "Roundtrip" do
      it "roundtrips a message to and from the MQTT broker" do
        -- Connect to the MQTT broker
        client <- liftEff $ connect url options

        -- Get an AVar and pass it off to the fiber running our MQTT process
        ref <- makeEmptyVar
        fiber <- forkAff $ runProcess $ mqttProcess client ref

        let topic   = Topic "test/purescript/mqtt"
        let message = Message "MQTT test message"

        -- Subscribe to the test topic, publish a message, and close the client
        liftEff $ subscribe client topic
        liftEff $ publish client topic message
        liftEff $ end client

        -- Verify that the topic and message received matches those sent
        msg <- takeVar ref
        msg.topic   `shouldEqual` unwrap topic
        msg.message `shouldEqual` unwrap message

        -- Get the final value from the coroutine process and verify that
        -- no exceptions were thrown.
        res <- try $ joinFiber fiber
        isLeft res `shouldEqual` false

--------------------------------------------------------------------------------
mqttConsumer
  :: AVar {topic :: String, message :: String}
  -> MQTTConsumer _ Unit
mqttConsumer ref = forever do
  e <- await
  case e of
    OnConnect -> pure unit
    OnClose   -> pure unit
    OnMessage t m ->
      lift $ putVar ({ topic : unwrap t, message : unwrap m}) ref

--------------------------------------------------------------------------------
mqttProcess
  :: ∀ e
   . Client
  -> AVar {topic :: String, message :: String}
  -> Process (Aff _) Unit
mqttProcess client ref = do
  let mqttProducer = mkMQTTProducer client
  mqttProducer $$ mqttConsumer ref

--------------------------------------------------------------------------------
url :: BrokerUrl
url = wrap "mqtt://localhost"

options :: Options
options =
  { port     : 1883
  , clientId : "purescript-mqtt-test"
  , username : mempty
  , password : mempty
  }
