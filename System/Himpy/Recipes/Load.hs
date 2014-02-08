module System.Himpy.Recipes.Load where
import System.Himpy.Recipes.Utils
import System.Himpy.Mib
import System.Himpy.Types
import System.Himpy.Logger
import System.Himpy.Output.Riemann
import Control.Concurrent.STM.TChan (TChan)

load_tuple :: (Int, Double) -> (String, Double)
load_tuple (x, y) = ("load-" ++ (show x), y)

load_rcp :: TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
load_rcp chan logchan (Host host comm _) = do
  loads <- snmp_walk_num host comm hrProcessorLoad
  let agg_load = (foldr (+) 0.0 loads) /
                 (fromIntegral $ length loads :: Double)
  let mtrs = snmp_metrics host "load" $ [("load-all", agg_load)]
  riemann_send chan mtrs
