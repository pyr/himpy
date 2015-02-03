module System.Himpy.Recipes.Network where
import System.Himpy.Recipes.Utils
import System.Himpy.Mib
import System.Himpy.Types
import System.Himpy.Logger
import System.Himpy.Output.Riemann
import System.Himpy.Index
import Control.Concurrent.STM.TChan (TChan)

hasMetric :: ((String,String), Maybe Double) -> Bool
hasMetric ((_,_), Nothing) = False
hasMetric ((_, _), (Just _)) = True

buildMetric ((host,service), (Just metric)) =
  Metric host service "ok" metric

net_rcp :: TChan ([Metric]) -> TChan (String) -> Integer -> HimpyHost -> HIndex -> IO ()
net_rcp chan logchan ival (Host host comm _) index = do
  names <- snmp_walk_str host comm ifName
  opstatus <- snmp_walk_num host comm ifOperStatus

  rx <- snmp_walk_num host comm ifInOctets
  tx <- snmp_walk_num host comm ifOutOctets

  let bw_in_keys = map (\x -> (host, x ++ " bandwidth in")) names
  let bw_out_keys = map (\x -> (host, x ++ " bandwidth out")) names

  all_bw_in <- mapM (\(x,y) -> (deriveMetric index ival x y)) (zip bw_in_keys rx)
  all_bw_out <- mapM (\(x,y) -> (deriveMetric index ival x y)) (zip bw_out_keys tx)

  let bw_in = [buildMetric t | t <- (zip bw_in_keys all_bw_in), hasMetric t]
  let bw_out = [buildMetric t | t <- (zip bw_out_keys all_bw_out), hasMetric t]

  conn <- snmp_walk_num host comm ifConnectorPresent
  adminstatus <- snmp_walk_num host comm ifAdminStatus

  let mtrs =  concat [snmp_metrics host "opstatus" $ zip names opstatus,
                      bw_in,
                      bw_out,
                      snmp_metrics host "conn" $ zip names conn,
                      snmp_metrics host "adminstatus" $ zip names adminstatus]
  riemann_send chan mtrs
