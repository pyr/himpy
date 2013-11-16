module Himpy.Mib where
import qualified Network.Protocol.NetSNMP as S

ifDescr :: S.RawOID
ifDescr = [1, 3, 6, 1, 2, 1, 2, 2, 1, 2]

ifInOctets :: S.RawOID
ifInOctets = [1, 3, 6, 1, 2, 1, 2, 2, 1, 10]

ifOutOctets :: S.RawOID
ifOutOctets = [1, 3, 6, 1, 2, 1, 2, 2, 1, 16]

ifOperStatus :: S.RawOID
ifOperStatus = [1, 3, 6, 1, 2, 1, 2, 2, 1, 8]

ifConnectorPresent :: S.RawOID
ifConnectorPresent = [1, 3, 6, 1, 2, 1, 2, 2, 1, 17]

ifAdminStatus :: S.RawOID
ifAdminStatus = [1, 3, 6, 1, 2, 1, 2, 2, 1, 7]

sysName :: S.RawOID
sysName = [1, 3, 6, 1, 2, 1, 1, 5, 0]

hrStorageDescr :: S.RawOID
hrStorageDescr = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 3]

hrStorageSize :: S.RawOID
hrStorageSize = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 5]

hrStorageUsed :: S.RawOID
hrStorageUsed = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 6]

hrStorageAllocationUnits :: S.RawOID
hrStorageAllocationUnits = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 4]

lanMgrServiceDescr :: S.RawOID
lanMgrServiceDescr = [1, 3, 6, 1, 4, 1, 77, 1, 2, 3, 1, 1]

lanMgrServiceStatus :: S.RawOID
lanMgrServiceStatus = [1, 3, 6, 1, 4, 1, 77, 1, 2, 3, 1, 3]

hrProcessorLoad :: S.RawOID
hrProcessorLoad = [1, 3, 6, 1, 2, 1, 25, 3, 3, 1, 2]

pfeCpuPercent :: S.RawOID
pfeCpuPercent = [1,3,6,1,4,1,2636,3,39,1,12,1,1,1,4,0]

pfeSessions :: S.RawOID
pfeSessions = [1,3,6,1,4,1,2636,3,39,1,12,1,1,1,6,0]

pfeSessionMax :: S.RawOID
pfeSessionMax = [1,3,6,1,4,1,2636,3,39,1,12,1,1,1,7,0]

pfePSU1 :: S.RawOID
pfePSU1 = [1,3,6,1,4,1,2636,3,1,15,1,8,2,1,0,0]

pfePSU2 :: S.RawOID
pfePSU2 = [1,3,6,1,4,1,2636,3,1,15,1,8,2,2,0,0]
