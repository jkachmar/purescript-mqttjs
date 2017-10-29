module Network.MQTT.Coroutine where

import Control.Bind (discard)
import Control.Coroutine (Producer, Consumer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Unit (Unit, unit)
import Network.MQTT (Client, MQTT, Message, Topic, onClose, onConnect, onMessage)
import Node.Buffer (BUFFER)

--------------------------------------------------------------------------------
-- | An Algebraic Data Type representing the events that a `Client` can emit.
data MQTTEvent
  = OnConnect
  | OnMessage Topic Message
  | OnClose

--------------------------------------------------------------------------------
-- | Type alias for a `Producer` of `MQTTEvent`s.
type MQTTProducer e a =
  Producer MQTTEvent (Aff (avar :: AVAR, buffer :: BUFFER, mqtt :: MQTT | e)) a

-- | Type alias for a `Consumer` of `MQTTEvent`s.
type MQTTConsumer e a =
  Consumer MQTTEvent (Aff e) a

--------------------------------------------------------------------------------
-- | Create a `Producer` of `MQTTEvent`s for a given `Client`.
mkMQTTProducer :: ∀ e. Client -> MQTTProducer e Unit
mkMQTTProducer client =
  produce \emit -> do
    onConnect client $ \_ ->
      emit <<< Left $ OnConnect

    onMessage client \t m ->
      emit <<< Left $ OnMessage t m

    onClose client $ \_ -> do
      emit <<< Left  $ OnClose
      emit <<< Right $ unit
