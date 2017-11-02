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
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Node.Buffer (BUFFER)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run)

--------------------------------------------------------------------------------
-- Alias for a record of AVars that we'll use to ferry information to/from the
-- spec runner and the MQTT coroutine.
type Ferry =
  { connected    :: AVar Boolean
  , disconnected :: AVar Boolean
  , topic        :: AVar String
  , message      :: AVar String
  }

--------------------------------------------------------------------------------
-- Alias for all the effects we'll need for this test.
type TestEffects e =
  ( avar    :: AVAR
  , buffer  :: BUFFER
  , console :: CONSOLE
  , mqtt    :: MQTT
  , process :: PROCESS
  | e
  )

--------------------------------------------------------------------------------
-- The HiveMQ public broker, which we'll use to test against.
url :: BrokerUrl
url = wrap "mqtt://broker.hivemq.com"

-- Connection options required to connect to the above broker.
options :: Options
options =
  { port     : 1883
  , clientId : "purescript-mqtt-test"
  , username : mempty
  , password : mempty
  }

--------------------------------------------------------------------------------
-- A coroutine `Consumer` that updates the relevant fields within `Ferry`
-- whenever events are received from the MQTT client.
mqttConsumer :: ∀ e. Ferry -> MQTTConsumer (TestEffects e) Unit
mqttConsumer {connected, disconnected, topic, message} = forever do
  e <- await
  case e of
    OnConnect -> lift $ putVar true connected
    OnClose   -> lift $ putVar true disconnected
    OnMessage t m -> lift $ do
      putVar (unwrap t) topic
      putVar (unwrap m) message

--------------------------------------------------------------------------------
-- A coroutine `Process` that creates a coroutine `Producer` for `MQTTEvent`s and
-- fuses its output to the `Consumer` defined above.
mqttProcess :: ∀ e
   . Client
  -> Ferry
  -> Process (Aff (TestEffects e)) Unit
mqttProcess client ferry = do
  let mqttProducer = mkMQTTProducer client
  mqttProducer $$ mqttConsumer ferry

--------------------------------------------------------------------------------
main :: ∀ e. Eff (TestEffects e) Unit
main = launchAff_ $ do
  -- Create an `AVar` to hold the information that needs to be communicated
  -- between `describe` blocks.
  scaffold <- makeEmptyVar

  liftEff $ run [consoleReporter] $ do
    describe "purescript-mqttjs" do
      describe "Roundtrip" do
        it "connects to the MQTT broker" do
          -- Connect to the MQTT broker.
          client <- liftEff $ connect url options

          -- Make empty AVars to ferry information to/from the coroutine.
          topic <- makeEmptyVar
          message <- makeEmptyVar
          connected <- makeEmptyVar
          disconnected <- makeEmptyVar
          let ferry = { topic
                      , message
                      , connected
                      , disconnected
                      }

          -- Run the coroutine process in a fiber and keep a reference to it to
          -- pass between tests.
          fiber <- forkAff $ runProcess $ mqttProcess client ferry
          putVar {client, ferry, fiber} scaffold

          -- Verify that we've successfully connected to the MQTT broker.
          connected <- takeVar $ ferry.connected
          connected `shouldEqual` true

        it "roundtrips a message to and from the MQTT broker" do
          {ferry, client} <- readVar scaffold

          let topic   = Topic "test/purescript/mqtt"
          let message = Message "MQTT test message"

          -- Subscribe to the test topic and publish a message on that topic.
          liftEff $ subscribe client topic
          liftEff $ publish client topic message

          -- Verify that the topic received matches the one published on.
          topic' <- takeVar $ ferry.topic
          topic' `shouldEqual` unwrap topic

          -- Verify that the message received matches the one published.
          message' <- takeVar $ ferry.message
          message' `shouldEqual` unwrap message

        it "disconnects from the MQTT broker" do
          {client, ferry, fiber} <- readVar scaffold

          -- Disconnect from the MQTT client.
          liftEff $ end client

          -- Verify that the the coroutine registered the disconnection event.
          disconnected <- takeVar $ ferry.disconnected
          disconnected `shouldEqual` true

          -- Get the final value from the coroutine process and verify that no
          -- exceptions were thrown.
          res <- try $ joinFiber fiber
          isLeft res `shouldEqual` false

