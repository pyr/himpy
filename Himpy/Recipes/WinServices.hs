module Himpy.Recipes.WinServices where
import Himpy.Recipes.Utils
import Himpy.Mib
import Himpy.Types
import Himpy.Logger
import Control.Concurrent.STM.TChan (TChan)
import qualified Data.Map as M

srv_rcp :: [String] -> TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
srv_rcp srvs chan logchan (Host host comm _) = do

  names <- snmp_walk_str host comm lanMgrServiceDescr
  statuses <- snmp_walk_num host comm lanMgrServiceStatus

  let seen = M.fromList $ zip names statuses
  let defaults = M.fromList $ zip srvs $ repeat 0.0

  let flat = M.assocs $ M.union seen defaults
  let mtrs = snmp_metrics host "service" flat

  log_info logchan $ "got snmp result: " ++ show (mtrs)
