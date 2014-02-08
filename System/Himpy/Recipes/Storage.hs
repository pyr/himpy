module System.Himpy.Recipes.Storage where
import System.Himpy.Recipes.Utils
import System.Himpy.Mib
import System.Himpy.Types
import System.Himpy.Logger
import System.Himpy.Output.Riemann
import Data.List
import Control.Concurrent.STM.TChan (TChan)

sanitize :: String -> String
sanitize input = h where (h,_) = break (== ':') input

storage_pct :: (Double, Double) -> Double
storage_pct (used,size) = (used / size) * 100

storage_realsize :: (Double, Double) -> Double
storage_realsize (size,allocunits) = size * allocunits

storage_rcp :: TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
storage_rcp chan logchan (Host host comm _) = do

  names <- snmp_walk_str host comm hrStorageDescr
  sizes <- snmp_walk_num host comm hrStorageSize
  used <- snmp_walk_num host comm hrStorageUsed
  allocs <- snmp_walk_num host comm hrStorageAllocationUnits

  let pcts = map storage_pct $ zip used sizes
  let mtrs = snmp_metrics host "percent" $ zip (map sanitize names) pcts
  riemann_send chan mtrs
