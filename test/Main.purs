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
import Data.Maybe (fromMaybe)
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

        liftEff $ subscribe client $ Topic "test/purescript-mqtt"
        liftEff $ publish client (Topic "test/purescript-mqtt") (Message "test")
        liftEff $ end client

        msg <- takeVar ref
        msg.topic   `shouldEqual` "test/purescript-mqtt"
        msg.message `shouldEqual` "test"

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
