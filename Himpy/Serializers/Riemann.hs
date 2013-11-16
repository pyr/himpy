{-# LANGUAGE DeriveGeneric #-}
module Himpy.Serializers.Riemann where
import Himpy.Types
import Himpy.Utils
import Data.Int
import Data.Text
import Data.ProtocolBuffers
import Data.Monoid (mempty)
import Data.Serialize (runPut)
import Data.Serialize.Put (Put)
import Data.ByteString as B
import Data.TypeLevel (D1, D2, D3, D4, D5, D6, D7, D8,
                       D9, D10, D11, D12, D13, D14, D15)
import GHC.Generics (Generic)

data ProtoAttr = ProtoAttr {
  key :: Optional D1 (Value Text),
  value :: Optional D2 (Value Text)
  } deriving (Generic, Show)

data ProtoEvent = ProtoEvent {
  time  :: Optional D1 (Value Int64),
  state :: Optional D2 (Value Text),
  service :: Optional D3 (Value Text),
  host :: Optional D4 (Value Text),
  description :: Optional D5 (Value Text),
  tags :: Repeated D7 (Value Text),
  ttl :: Optional D8 (Value Double),

  attributes :: Repeated D9 (Message ProtoAttr),

  metric_sint64 :: Optional D13 (Value Int64),
  metric_d      :: Optional D14 (Value Double),
  metric_f      :: Optional D15 (Value Double)
  } deriving (Generic, Show)

data ProtoQuery = ProtoQuery {
  query :: Optional D1 (Value Text)
  } deriving (Generic, Show)


data ProtoState = ProtoState {
  s_time :: Optional D1 (Value Int64),
  s_state :: Optional D2 (Value Text),
  s_service :: Optional D3 (Value Text),
  s_host :: Optional D4 (Value Text),
  s_description :: Optional D5 (Value Text),
  s_once :: Optional D6 (Value Bool),
  s_tags :: Repeated D7 (Value Text),
  s_ttl :: Optional D8 (Value Double)
  } deriving (Generic, Show)

data ProtoMsg = ProtoMsg {
  m_ok :: Optional D2 (Value Bool),
  m_error :: Optional D3 (Value Text),
  m_states :: Repeated D4 (Message ProtoState),
  m_query :: Optional D5 (Message ProtoQuery),
  m_events :: Repeated D6  (Message ProtoEvent)
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

metric_to_event :: Metric -> IO (B.ByteString)
metric_to_event (Metric host service metric) = do
  tstamp <- timestamp
  let event = ProtoEvent  {
        time = putField $ Just (fromIntegral tstamp :: Int64),
        state = putField mempty,
        service = putField $ Just $ pack_text service,
        host = putField $ Just $ pack_text host,
        description = putField Nothing,
        tags = putField [pack_text "snmp"],
        ttl = putField $ Just 120,
        attributes = putField mempty,
        metric_sint64 = putField Nothing,
        metric_d = putField $ Just metric,
        metric_f = putField Nothing
        }
  let as_put = encodeMessage event
  let encoded = runPut as_put
  return (encoded)

metric_to_protoevent :: Integer -> Metric -> ProtoEvent
metric_to_protoevent tstamp (Metric host service metric) =
  ProtoEvent {
          time = putField $ Just (fromIntegral tstamp :: Int64),
          state = putField mempty,
          service = putField $ Just $ pack_text service,
          host = putField $ Just $ pack_text host,
          description = putField Nothing,
          tags = putField [pack_text "snmp", pack_text "himpy"],
          ttl = putField $ Just 120,
          attributes = putField mempty,
          metric_sint64 = putField Nothing,
          metric_d = putField $ Just metric,
          metric_f = putField Nothing
          }

metrics_to_msg :: [Metric] -> IO (B.ByteString)
metrics_to_msg metrics = do
  tstamp <- timestamp
  let events = [metric_to_protoevent tstamp m | m <- metrics]
  let msg = ProtoMsg {
        m_ok = putField $ Just True,
        m_error = putField mempty,
        m_query = putField Nothing,
        m_states = putField mempty,
        m_events = putField events
        }
  let encoded = runPut $ encodeMessage msg
  return encoded
