module System.Himpy.Recipes.Utils where
import System.Himpy.Types
import Data.Word
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Network.Protocol.NetSNMP as Snmp

non_nul :: Char -> Bool
non_nul '\0' = False
non_nul _ = True

snmp_metric :: String -> String -> (String, Double) -> Metric
snmp_metric host suffix (ifname,metric) =
  Metric host (ifname ++ " " ++ suffix) "ok" metric

snmp_metrics :: String -> String -> [(String, Double)] -> [Metric]
snmp_metrics host suffix metrics =
  [snmp_metric host suffix m | m <- metrics]

snmp_last_str :: Snmp.ASNValue -> String
snmp_last_str v =  Snmp.showASNValue v

snmp_last_strs :: Either String [Snmp.SnmpResult] -> [String]
snmp_last_strs (Right results) =
  [show r | r <- results]

snmp_num :: Snmp.ASNValue -> Double
snmp_num (Snmp.Integer32 v) = fromIntegral vprime :: Double where
  vprime = fromIntegral v :: Word32
snmp_num (Snmp.Integer64 v) = fromIntegral v :: Double
snmp_num (Snmp.Counter32 v) = fromIntegral v :: Double
snmp_num (Snmp.Counter64 v) = fromIntegral v :: Double
snmp_num (Snmp.Unsigned32 v) = fromIntegral v :: Double
snmp_num (Snmp.Unsigned64 v) = fromIntegral v :: Double
snmp_num (Snmp.Gauge32 v) = fromIntegral v :: Double

snmp_nums :: Either String [Snmp.SnmpResult] -> [Double]
snmp_nums (Right results) =
  [snmp_num value | (Snmp.SnmpResult oid value) <- results]

snmp_str :: Snmp.ASNValue -> String
snmp_str (Snmp.OctetString v _) = filter non_nul $ B.unpack v
snmp_str v = "unknown: " ++ (Snmp.showASNValue v)

snmp_strs :: [Snmp.SnmpResult] -> [String]
snmp_strs results =
  [snmp_str value | (Snmp.SnmpResult oid value) <- results]

snmp_walk_str :: String -> String -> Snmp.RawOID -> IO ([String])
snmp_walk_str host comm oid = do
  v <- Snmp.snmpWalk Snmp.snmp_version_2c (B.pack host) (B.pack comm) oid
  case v of
    Left error -> do
      -- XXX: should log here
      return []
    Right results ->
      return (snmp_strs results)

snmp_walk_num :: String -> String -> Snmp.RawOID -> IO ([Double])
snmp_walk_num host comm oid = do
  v <- Snmp.snmpWalk Snmp.snmp_version_2c (B.pack host) (B.pack comm) oid
  return (snmp_nums v)

snmp_walk_last_str :: String -> String -> Snmp.RawOID -> IO ([String])
snmp_walk_last_str host comm oid = do
  v <- Snmp.snmpWalk Snmp.snmp_version_2c (B.pack host) (B.pack comm) oid
  return (snmp_last_strs v)

snmp_oid :: Snmp.ASNValue -> Snmp.RawOID
snmp_oid (Snmp.OID _ _ o) = [fromIntegral subo :: Snmp.OIDpart | subo <- o]

snmp_oids :: [Snmp.SnmpResult] -> [Snmp.RawOID]
snmp_oids results = [snmp_oid value | (Snmp.SnmpResult oid value) <- results]

snmp_walk_oid :: String -> String -> Snmp.RawOID -> IO ([Snmp.RawOID])
snmp_walk_oid host comm oid = do
  v <- Snmp.snmpWalk Snmp.snmp_version_2c (B.pack host) (B.pack comm) oid
  case v of
    Left error -> return []
    Right results -> return (snmp_oids results)

snmp_get_num :: String -> String -> Snmp.RawOID -> IO (Double)
snmp_get_num host comm oid = do
  v <- Snmp.snmpGet Snmp.snmp_version_2c (B.pack host) (B.pack comm) oid
  let out = case v of (Right (Snmp.SnmpResult oid value)) -> snmp_num value
  return out
