{-# LANGUAGE ScopedTypeVariables #-}
module Himpy.Output.Riemann (riemann_start, riemann_send) where
import Himpy.Types
import Himpy.Utils
import Himpy.Logger
import Himpy.Serializers.Riemann
import Network (connectTo, PortID(PortNumber))
import Network.Socket (withSocketsDo,
                       Socket,
                       SockAddr(SockAddrInet),
                       inet_addr,
                       Family(AF_INET),
                       SocketType(Datagram, Stream),
                       defaultProtocol,
                       socket)
import Network.Socket.ByteString (sendAllTo)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan, readTChan, newTChanIO, TChan)
import System.IO
import Data.Binary.Get (runGet, getWord32be)
import Data.Word (Word32)
import Control.Exception
import Text.Regex.Posix
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

-- Simplistic riemann write module
to_state :: Metric -> String -> Metric
to_state (Metric host service _ point) state =
  (Metric host service state point)

match_threshold :: Metric -> Threshold -> Bool
match_threshold (Metric host service _ _) (Threshold { tHost = h,
                                                       tService = s }) =
  ((host =~ h) && (service =~ s))

find_threshold :: [Threshold] -> Metric -> Maybe Threshold
find_threshold thresholds metric =
  case filter (match_threshold metric) thresholds of
    [] -> Nothing
    (threshold:_) -> Just threshold

apply_threshold :: Maybe Threshold -> Metric -> Metric
apply_threshold Nothing metric= metric
apply_threshold (Just (Threshold {tWarning = Just warn, tCritical = crit})) metric =
  if point >= crit then
    to_state metric "critical"
  else if point >= warn then to_state metric "warning"
       else metric
  where (Metric _ _ _ point) = metric


apply_thresholds :: [Threshold] -> Metric -> Metric
apply_thresholds thresholds metric = apply_threshold threshold metric where
  threshold = find_threshold thresholds metric

riemann_write_out fd hmsg = do
  B.hPut fd hmsg
  hFlush fd
  -- wait for ack now
  raw_sz <- B.hGet fd 4
  let sz = runGet getWord32be $ BL.fromChunks [raw_sz]
  -- no deserialization of payloads for now
  B.hGet fd (fromIntegral sz)
  return ()

riemann_safe_write logchan host port hmsg = do
  let handler = (\(e :: SomeException) -> log_info logchan $ "send error: " ++ (show e))
  fd <- connectTo host (PortNumber port)
  riemann_write_out fd hmsg `catch` handler `finally` hClose fd
  return ()

riemann_write :: TChan String -> TChan [Metric] -> [Threshold] -> String -> IO ()
riemann_write logchan chan thresholds host = do
  raw_metrics <- atomically $ readTChan chan
  let metrics = map (apply_thresholds thresholds) raw_metrics
  msg <- metrics_to_msg metrics

  let hdr = B.pack $ octets $ (fromIntegral (B.length msg) :: Word32)
  let hmsg = B.concat [hdr, msg]
  log_info logchan $ "sending out: " ++ (show $ length metrics)

  let handler = (\(e :: SomeException) -> log_info logchan $ "write error: " ++ (show e))

  riemann_safe_write logchan host 5555 hmsg `catch` handler
  return ()

riemann_start :: TChan String -> String -> [Threshold] -> IO (TChan [Metric])
riemann_start logchan host thresholds = do
  chan <- newTChanIO
  void $ forkIO $ forever $ riemann_write logchan chan thresholds host
  return (chan)

riemann_send :: TChan [Metric] -> [Metric] -> IO ()
riemann_send chan metrics = do
  atomically $ writeTChan chan metrics
