module Himpy.Output.Riemann (riemann_start, riemann_send) where
import Himpy.Types
import Himpy.Utils
import Himpy.Logger
import Himpy.Serializers.Riemann

import Network.Socket (withSocketsDo,
                       Socket,
                       SockAddr(SockAddrInet),
                       inet_addr,
                       Family(AF_INET),
                       SocketType(Datagram),
                       defaultProtocol,
                       socket)
import Network.Socket.ByteString (sendAllTo)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan, readTChan, newTChanIO, TChan)

-- Simplistic logging module

riemann_write :: TChan String -> TChan [Metric] -> Socket -> String -> IO ()
riemann_write logchan chan s host = do
  metrics <- atomically $ readTChan chan
  msg <- metrics_to_msg metrics
  hostAddr <- inet_addr host
  let dest = (SockAddrInet 5555 hostAddr)
  log_info logchan $ "sending out: " ++ (show $ length metrics)
  sendAllTo s msg dest
  return ()

riemann_start :: TChan String -> String -> IO (TChan [Metric])
riemann_start logchan host = withSocketsDo $ do
  chan <- newTChanIO
  s <- socket AF_INET Datagram defaultProtocol
  void $ forkIO $ forever $ riemann_write logchan chan s host
  return (chan)

riemann_send :: TChan [Metric] -> [Metric] -> IO ()
riemann_send chan metrics = do
  atomically $ writeTChan chan metrics
