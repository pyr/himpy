{-# LANGUAGE OverloadedStrings #-}
module System.Himpy.Config (configure) where
import System.Himpy.Types
import Control.Applicative
import Data.Aeson
import System.IO (readFile)
import Data.Text (unpack, Text)

import Data.HashMap.Strict (member)
import Data.Attoparsec.Number as AN
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL

instance FromJSON LevelThreshold where
  parseJSON (Object o) | (member "min" o && member "max" o) = LevelThreshold <$> o .: "min"
                                                                             <*> o .: "max"
                       | (member "min" o) = LevelThreshold <$> o .: "min"
                                                           <*> pure Nothing
                       | (member "max" o) = LevelThreshold <$> pure Nothing
                                                           <*> o .: "max"
  parseJSON (Number d) = pure (LevelThreshold Nothing (Just (fromRational (toRational d))))

instance FromJSON Threshold where
  parseJSON (Object o) | member "warning" o = Threshold <$> o .: "host"
                                                        <*> o .: "service"
                                                        <*> o .: "warning"
                                                        <*> o .: "critical"
                       | otherwise = Threshold <$> o .: "host"
                                               <*> o .: "service"
                                               <*> pure Nothing
                                               <*> o .: "critical"

instance FromJSON HimpyRecipe where
  parseJSON (String "load") = pure LoadRecipe
  parseJSON (String "storage") = pure StorageRecipe
  parseJSON (String "juniper") = pure JuniperRecipe
  parseJSON (String "iostat") = pure IostatRecipe
  parseJSON (String "network") = pure NetworkRecipe
  parseJSON (Object o) | member "winservices" o = WinSrvRecipe <$> o .: "winservices"
                       | member "network" o = pure NetworkRecipe
                       | member "iostat" o = pure IostatRecipe
                       | member "storage" o = pure StorageRecipe
                       | member "load" o = pure LoadRecipe
                       | member "juniper" o = pure JuniperRecipe

instance FromJSON HimpyHost where
  parseJSON (Object o) = Host <$> o .: "host"
                              <*> o .: "community"
                              <*> o .: "recipes"

instance FromJSON HimpyConfig where
  parseJSON (Object o) = Hosts <$> o .: "interval"
                               <*> o .: "ttl"
                               <*> o .: "host"
                               <*> o .: "port"
                               <*> o .: "logfile"
                               <*> o .: "hosts"
                               <*> o .: "thresholds"

configure path = do
   content <- BL.readFile path
   let parsed = eitherDecode content :: Either String HimpyConfig
   putStrLn $ "got config: " ++ show parsed
   let (Right conf) = parsed
   return conf
