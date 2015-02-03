{-# LANGUAGE DeriveGeneric, DataKinds #-}
module System.Himpy.Serializers.Riemann where
import System.Himpy.Types
import System.Himpy.Utils
import Data.Int
import Data.Text
import Data.ProtocolBuffers
import Data.Monoid (mempty)
import Data.Serialize (runPut)
import Data.Serialize.Put (Put)
import Data.ByteString as B
import GHC.Generics (Generic)
import GHC.TypeLits

data ProtoAttr = ProtoAttr {
  key :: Optional 1 (Value Text),
  value :: Optional 2 (Value Text)
  } deriving (Generic, Show)

data ProtoEvent = ProtoEvent {
  time  :: Optional 1 (Value Int64),
  state :: Optional 2 (Value Text),
  service :: Optional 3 (Value Text),
  host :: Optional 4 (Value Text),
  description :: Optional 5 (Value Text),
  tags :: Repeated 6 (Value Text),
  ttl :: Optional 7 (Value Float),

  attributes :: Repeated 8 (Message ProtoAttr),

  metric_sint64 :: Optional 13 (Value Int64),
  metric_d      :: Optional 14 (Value Double),
  metric_f      :: Optional 15 (Value Double)
  } deriving (Generic, Show)

data ProtoQuery = ProtoQuery {
  query :: Optional 1 (Value Text)
  } deriving (Generic, Show)


data ProtoState = ProtoState {
  s_time :: Optional 1 (Value Int64),
  s_state :: Optional 2 (Value Text),
  s_service :: Optional 3 (Value Text),
  s_host :: Optional 4 (Value Text),
  s_description :: Optional 5 (Value Text),
  s_once :: Optional 6 (Value Bool),
  s_tags :: Repeated 7 (Value Text),
  s_ttl :: Optional 8 (Value Double)
  } deriving (Generic, Show)

data ProtoMsg = ProtoMsg {
  m_ok :: Optional 2 (Value Bool),
  m_error :: Optional 3 (Value Text),
  m_states :: Repeated 4 (Message ProtoState),
  m_query :: Optional 5 (Message ProtoQuery),
  m_events :: Repeated 6  (Message ProtoEvent)
} deriving (Generic, Show)


instance Encode ProtoAttr
instance Decode ProtoAttr
instance Encode ProtoEvent
instance Decode ProtoEvent
instance Encode ProtoMsg
instance Decode ProtoMsg
instance Encode ProtoQuery
instance Decode ProtoQuery
instance Encode ProtoState
instance Decode ProtoState


pack_text = Data.Text.pack

metric_to_protoevent :: Integer -> Metric -> Float -> ProtoEvent
metric_to_protoevent tstamp (Metric host service state metric) dttl =
  ProtoEvent {
          time = putField $ Just (fromIntegral tstamp :: Int64),
          state = putField $ Just $ pack_text state,
          service = putField $ Just $ pack_text service,
          host = putField $ Just $ pack_text host,
          description = putField Nothing,
          tags = putField [pack_text "snmp", pack_text "himpy"],
          ttl = putField $ (Just dttl),
          attributes = putField mempty,
          metric_sint64 = putField Nothing,
          metric_d = putField $ Just metric,
          metric_f = putField Nothing
          }

metrics_to_msg :: [Metric] -> Float -> IO (B.ByteString)
metrics_to_msg metrics dttl = do
  tstamp <- timestamp
  let events = [metric_to_protoevent tstamp m dttl | m <- metrics]
  let msg = ProtoMsg {
        m_ok = putField $ Just True,
        m_error = putField mempty,
        m_query = putField Nothing,
        m_states = putField mempty,
        m_events = putField events
        }
  let encoded = runPut $ encodeMessage msg
  return encoded
