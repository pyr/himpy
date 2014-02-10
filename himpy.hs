import System.Himpy.Config (configure)
import System.Himpy.Logger (log_start, log_info)
import System.Himpy.Recipes
import System.Himpy.Types
import System.Himpy.Serializers.Riemann
import System.Himpy.Output.Riemann
import System.Himpy.Index
import System.IO
import System.Environment
import Data.List (concatMap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, TChan)
import qualified Data.ByteString.Char8 as B
import qualified Network.Protocol.NetSNMP as Snmp

collect_profiles :: TChan ([Metric]) -> TChan (String) -> Integer -> HimpyHost -> HIndex -> IO ()
collect_profiles chan logchan ival (Host host comm (prof:profs)) index = do
  recipe prof chan logchan (Host host comm [prof]) index
  collect_profiles chan logchan ival (Host host comm profs) index
collect_profiles chan logchan ival (Host host _ []) index = do
  log_info logchan $ "ticking for host: " ++ host
  threadDelay (fromIntegral (ival * 1000000) :: Int)

start_collector ::  TChan ([Metric]) -> TChan (String) -> Integer -> HimpyHost -> HIndex -> IO ()
start_collector chan logchan ival host index =
  void $ forkIO $ forever $ collect_profiles chan logchan ival host index

start_collectors :: TChan ([Metric]) -> TChan (String) -> HimpyConfig -> HIndex -> IO ()
start_collectors chan logchan (Hosts ival t x y z (host:hosts) _) index = do
  start_collector chan logchan ival host index
  start_collectors chan logchan (Hosts ival t x y z hosts []) index
start_collectors chan logchan (Hosts _ _ _ _ _ [] _) _ = do return ()

get_conf_path [] = "/etc/himpy.conf"
get_conf_path (path:_) = path

main :: IO ()
main = do
  Snmp.initialize
  index <- initIndex
  args <- getArgs
  config <- configure $ get_conf_path args
  let (Hosts intval ttl host port logfile _ thresholds) = config
  logchan <- log_start logfile
  chan <- riemann_start logchan host port ttl thresholds
  log_info logchan "starting himpy"
  start_collectors chan logchan config index
  void $ forever $ threadDelay (fromIntegral (intval * 1000000) :: Int)
