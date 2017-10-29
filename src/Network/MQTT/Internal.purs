module Network.MQTT.Internal where

import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Newtype (class Newtype)
import Node.Buffer (BUFFER, toString)
import Node.Encoding (Encoding(..))

--------------------------------------------------------------------------------
-- | A topic that can be subscribed to.
newtype Topic = Topic String
derive instance ntTopic :: Newtype Topic _

-- | A message that can be sent to a topic, or received from one.
newtype Message = Message String
derive instance ntMessage :: Newtype Message _

--------------------------------------------------------------------------------
-- | Unsafely coerce a `Foreign` value to a `Topic`.
unsafeReadTopic :: Foreign -> Topic
unsafeReadTopic t = Topic $ unsafeFromForeign t

-- | Unsafely coerce a `Foreign` value to a `Buffer` and read it as a
-- | UTF8-encoded `Message`.
unsafeReadMessage :: ∀ e. Foreign -> Eff (buffer :: BUFFER | e) Message
unsafeReadMessage m = Message <$> (toString UTF8 $ unsafeFromForeign m)
