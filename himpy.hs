import Himpy.Config (configure)
import Himpy.Logger (log_start, log_info)
import Himpy.Recipes
import Himpy.Types
import Himpy.Serializers.Riemann
import Himpy.Output.Riemann
import System.IO
import Data.List (concatMap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, TChan)
import qualified Data.ByteString.Char8 as B
import qualified Network.Protocol.NetSNMP as Snmp

collect_profiles :: TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
collect_profiles chan logchan (Host host comm (prof:profs)) = do
  recipe prof chan logchan (Host host comm [prof])
  collect_profiles chan logchan (Host host comm profs)
collect_profiles chan logchan (Host host _ []) = do
  log_info logchan $ "ticking for host: " ++ host
  threadDelay 10000000

start_collector ::  TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
start_collector chan logchan host =
  void $ forkIO $ forever $ collect_profiles chan logchan host

start_collectors :: TChan ([Metric]) -> TChan (String) -> HimpyConfig -> IO ()
start_collectors chan logchan (Hosts (host:hosts) _) = do
  start_collector chan logchan host
  start_collectors chan logchan (Hosts hosts [])
start_collectors chan logchan (Hosts [] _) = do return ()

main :: IO ()
main = do
  Snmp.initialize
  config <- configure "/etc/himpy.conf"
  let Hosts _ thresholds = config
  logchan <- log_start "/var/log/himpy.log"
  chan <- riemann_start logchan "127.0.0.1" thresholds
  log_info logchan "starting himpy"
  start_collectors chan logchan config
  void $ forever $ threadDelay 10000000
