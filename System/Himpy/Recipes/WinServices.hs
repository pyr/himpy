module System.Himpy.Recipes.WinServices where
import System.Himpy.Recipes.Utils
import System.Himpy.Mib
import System.Himpy.Types
import System.Himpy.Logger
import System.Himpy.Output.Riemann
import Control.Concurrent.STM.TChan (TChan)
import qualified Data.Map as M

srv_rcp :: [String] -> TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
srv_rcp srvs chan logchan (Host host comm _) = do

  names <- snmp_walk_str host comm lanMgrServiceDescr
  statuses <- snmp_walk_num host comm lanMgrServiceStatus

  let seen = M.fromList $ zip names statuses
  let defaults = M.fromList $ zip srvs $ repeat 0.0

  let flat = M.assocs $ M.union seen defaults
  riemann_send chan $ snmp_metrics host "service" flat
