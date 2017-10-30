module Test.Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Console
import Network.MQTT
import Network.MQTT.Coroutine

import Control.Coroutine (Process, await, runProcess, ($$))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

--------------------------------------------------------------------------------
main :: Eff _ Unit
main = run [consoleReporter] do
  describe "purescript-mqtt" do
    describe "Roundtrip" do
      it "should send and recover a message to/from the test broker" do
        client <- liftEff $ connect url options

        ref <- makeEmptyVar
        _   <- forkAff $ runProcess $ mqttProcess client ref

        let topic   = Topic "test/purescript/mqtt"
        let message = Message "MQTT test message"

        liftEff $ subscribe client topic
        liftEff $ publish client topic message
        liftEff $ end client

        msg <- takeVar ref
        msg.topic   `shouldEqual` unwrap topic
        msg.message `shouldEqual` unwrap message

--------------------------------------------------------------------------------
mqttConsumer
  :: AVar {topic :: String, message :: String}
  -> MQTTConsumer _ Unit
mqttConsumer ref = forever do
  e <- await
  case e of
    OnConnect -> pure unit
    OnClose -> pure unit
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
url = wrap "mqtt://broker.hivemq.com"

options :: Options
options =
  { port     : 1883
  , clientId : "purescript-mqtt-test"
  , username : mempty
  , password : mempty
  }
