module System.Himpy.Recipes.Network where
import System.Himpy.Recipes.Utils
import System.Himpy.Mib
import System.Himpy.Types
import System.Himpy.Logger
import System.Himpy.Output.Riemann
import Control.Concurrent.STM.TChan (TChan)

net_rcp :: TChan ([Metric]) -> TChan (String) -> HimpyHost -> IO ()
net_rcp chan logchan (Host host comm _) = do
  names <- snmp_walk_str host comm ifName
  opstatus <- snmp_walk_num host comm ifOperStatus
  rx <- snmp_walk_num host comm ifInOctets
  tx <- snmp_walk_num host comm ifOutOctets
  conn <- snmp_walk_num host comm ifConnectorPresent
  adminstatus <- snmp_walk_num host comm ifAdminStatus

  let mtrs =  concat [snmp_metrics host "opstatus" $ zip names opstatus,
                      snmp_metrics host "rx" $ zip names rx,
                      snmp_metrics host "tx" $ zip names tx,
                      snmp_metrics host "conn" $ zip names conn,
                      snmp_metrics host "adminstatus" $ zip names adminstatus]
  riemann_send chan mtrs
