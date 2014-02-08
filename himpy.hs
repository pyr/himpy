import System.Himpy.Config (configure)
import System.Himpy.Logger (log_start, log_info)
import System.Himpy.Recipes
import System.Himpy.Types
import System.Himpy.Serializers.Riemann
import System.Himpy.Output.Riemann
import System.IO
import System.Environment
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
start_collectors chan logchan (Hosts x y z (host:hosts) _) = do
  start_collector chan logchan host
  start_collectors chan logchan (Hosts x y z hosts [])
start_collectors chan logchan (Hosts _ _ _ [] _) = do return ()

get_conf_path [] = "/etc/himpy.conf"
get_conf_path (path:_) = path

main :: IO ()
main = do
  Snmp.initialize
  args <- getArgs
  config <- configure $ get_conf_path args
  let (Hosts host port logfile _ thresholds) = config
  logchan <- log_start logfile
  chan <- riemann_start logchan host port thresholds
  log_info logchan "starting himpy"
  start_collectors chan logchan config
  void $ forever $ threadDelay 10000000
