module Himpy.Mib where
import Network.Protocol.NetSNMP (RawOID)

ifDescr = [1, 3, 6, 1, 2, 1, 2, 2, 1, 2] :: RawOID
ifName = [1, 3, 6, 1, 2, 1, 31, 1, 1, 1, 1 ] :: RawOID
ifInOctets = [1, 3, 6, 1, 2, 1, 2, 2, 1, 10] :: RawOID
ifOutOctets = [1, 3, 6, 1, 2, 1, 2, 2, 1, 16] :: RawOID
ifOperStatus = [1, 3, 6, 1, 2, 1, 2, 2, 1, 8] :: RawOID
ifConnectorPresent = [1, 3, 6, 1, 2, 1, 2, 2, 1, 17] :: RawOID
ifAdminStatus = [1, 3, 6, 1, 2, 1, 2, 2, 1, 7] :: RawOID
sysName = [1, 3, 6, 1, 2, 1, 1, 5, 0] :: RawOID
hrStorageDescr = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 3] :: RawOID
hrStorageSize = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 5] :: RawOID
hrStorageUsed = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 6] :: RawOID
hrStorageAllocationUnits = [1, 3, 6, 1, 2, 1, 25, 2, 3, 1, 4] :: RawOID
lanMgrServiceDescr = [1, 3, 6, 1, 4, 1, 77, 1, 2, 3, 1, 1] :: RawOID
lanMgrServiceStatus = [1, 3, 6, 1, 4, 1, 77, 1, 2, 3, 1, 3] :: RawOID
hrProcessorLoad = [1, 3, 6, 1, 2, 1, 25, 3, 3, 1, 2] :: RawOID
pfeCpuPercent = [1,3,6,1,4,1,2636,3,39,1,12,1,1,1,4,0] :: RawOID
pfeSessions = [1,3,6,1,4,1,2636,3,39,1,12,1,1,1,6,0] :: RawOID
pfeSessionMax = [1,3,6,1,4,1,2636,3,39,1,12,1,1,1,7,0] :: RawOID
pfePSU1 = [1,3,6,1,4,1,2636,3,1,15,1,8,2,1,0,0] :: RawOID
pfePSU2 = [1,3,6,1,4,1,2636,3,1,15,1,8,2,2,0,0] :: RawOID
hrSystemProcesses = [1,3,6,1,2,1,25,1,6,0] :: RawOID
ciscoCpuRouting = [1,3,6,1,4,1,9,9,109,1,1,1,1,8,1] :: RawOID
ciscoCpuSwitching = [1,3,6,1,4,1,9,9,109,1,1,1,1,8,2] :: RawOID
ciscoMemAvail = [1,3,6,1,4,1,9,9,48,1,1,1,6,1] :: RawOID
ciscoMemUsed = [1,3,6,1,4,1,9,9,48,1,1,1,5,1] :: RawOID
ciscoSwitchCpu = [1,3,6,1,4,1,9,9,109,1,1,1,1,7,1] :: RawOID
ciscoSwitchMem = [1,3,6,1,4,1,9,9,305,1,1,2,0] :: RawOID
ciscoRouterPSU1 = [1,3,6,1,4,1,9,9,13,1,5,1,3,1] :: RawOID
ciscoRouterPSU2 = [1,3,6,1,4,1,9,9,13,1,5,1,3,2] :: RawOID
ciscoSwitchPSU1 = [1,3,6,1,4,1,9,9,117,1,1,2,1,2,470] :: RawOID
ciscoSwitchPSU2 = [1,3,6,1,4,1,9,9,117,1,1,2,1,2,471] :: RawOID
zfsPoolDescr = [1,3,6,1,4,1,25359,1,1,1] :: RawOID
zfsPoolHealth = [1,3,6,1,4,1,25359,1,1,4] :: RawOID
