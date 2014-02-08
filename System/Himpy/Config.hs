{-# LANGUAGE OverloadedStrings #-}
module System.Himpy.Config (configure) where
import System.Himpy.Types
import Data.Aeson
import System.IO (readFile)
import Data.Text (unpack, Text)

import Data.Attoparsec.Number as AN
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL

get_rcp :: (String,[String]) -> HimpyRecipe
get_rcp ("storage",_) = StorageRecipe
get_rcp ("network", _) = NetworkRecipe
get_rcp ("load", _) = LoadRecipe
get_rcp ("juniper", _) = JuniperRecipe
get_rcp ("winservices", services) = WinSrvRecipe services

to_str (String t) = unpack t

clean_rcp :: (Text, Value) -> (String, [String])
clean_rcp (k, (Array strs)) = (unpack k, V.toList $ V.map to_str strs)



get_host_conf :: Value -> HimpyHost
get_host_conf (Object h) = (Host (unpack host) (unpack comm) rcps) where
  (String host) = h HM.! "host"
  (String comm) = h HM.! "community"
  (Object raw_rcps) = h HM.! "recipes"
  rcps = map (get_rcp . clean_rcp) $ HM.toList raw_rcps

get_host :: Object -> String
get_host conf = case HM.lookup "host" conf of
   Nothing -> "127.0.0.1"
   Just (String x) -> unpack x

get_log :: Object -> String
get_log conf = case HM.lookup "logfile" conf of
   Nothing -> "/var/log/himpy.log"
   Just (String x) -> unpack x

get_port :: Object -> Integer
get_port conf = case HM.lookup "port" conf of
   Nothing -> 5555
   Just (String x) -> (read (unpack x) :: Integer)

get_hosts conf = case HM.lookup "hosts" conf of
   Nothing -> []
   Just (Array hosts) -> V.toList $ V.map get_host_conf hosts

get_warn :: Object -> Maybe Double
get_warn t = case HM.lookup "warning" t of
   Nothing -> Nothing
   Just (String w) -> Just (read (unpack w) :: Double)

get_threshold :: Value -> Threshold
get_threshold (Object t) =  Threshold {tHost = host, tService = service, tWarning =
                                          warning, tCritical = critical} where
  (String t_host) = t HM.! "host"
  (String t_service) = t HM.! "service"
  (String t_critical) = t HM.! "critical"
  host = unpack t_host
  service = unpack t_service
  warning = get_warn t
  critical =  (read (unpack t_critical) :: Double)


get_thresholds conf = case HM.lookup "thresholds" conf of
   Nothing -> []
   Just (Array thresholds) -> V.toList $ V.map get_threshold thresholds

from_json :: Object -> HimpyConfig
from_json conf =
   (Hosts (get_host conf) (get_port conf) (get_log conf)
    (get_hosts conf) (get_thresholds conf))


configure path = do
   content <- BL.readFile path
   let Just json_config = decode content :: Maybe Object
   let conf = from_json json_config
   return conf
