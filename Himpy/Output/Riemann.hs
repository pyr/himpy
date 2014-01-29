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
import Data.Word (Word32)
import Control.Exception
import qualified Data.ByteString as B

-- Simplistic riemann write module

riemann_write_out fd hmsg = do
  B.hPut fd hmsg
  hFlush fd

riemann_write :: TChan String -> TChan [Metric] -> String -> IO ()
riemann_write logchan chan host = do
  metrics <- atomically $ readTChan chan
  msg <- metrics_to_msg metrics
  let hdr = B.pack $ octets $ (fromIntegral (B.length msg) :: Word32)
  let hmsg = B.concat [hdr, msg]
  log_info logchan $ "sending out: " ++ (show $ length metrics)

  fd <- connectTo host (PortNumber 5555)
  let handler = (\(e :: SomeException) -> log_info logchan $ "send error: " ++ (show e))
  riemann_write_out fd hmsg `catch` handler `finally` hClose fd
  return ()

riemann_start :: TChan String -> String -> IO (TChan [Metric])
riemann_start logchan host = do
  chan <- newTChanIO
  void $ forkIO $ forever $ riemann_write logchan chan host
  return (chan)

riemann_send :: TChan [Metric] -> [Metric] -> IO ()
riemann_send chan metrics = do
  atomically $ writeTChan chan metrics
