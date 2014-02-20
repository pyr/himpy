module System.Himpy.Recipes.Iostat where
import System.Himpy.Recipes.Utils
import System.Himpy.Mib
import System.Himpy.Types
import System.Himpy.Logger
import System.Himpy.Output.Riemann
import Control.Concurrent.STM.TChan (TChan)

iostat_tuple :: (String, Double) -> (String, Double)
iostat_tuple (x, y) = ("iostat-" ++ x, y)

iostat_rcp :: TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
iostat_rcp chan logchan (Host host comm _) = do
  names <- snmp_walk_str host comm ioStatDisks
  errors <- snmp_walk_num host comm ioStatErrors

  let agg_errors = (foldr (+) 0.0 errors)
  let mtrs = snmp_metrics host "iostat" $ [iostat_tuple t | t <- zip names errors] ++ [("iostat-all", agg_errors)]
  riemann_send chan mtrs
