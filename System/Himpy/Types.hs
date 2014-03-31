module System.Himpy.Types where
import Control.Concurrent.MVar (MVar)
import Data.Map (Map)
import Control.Concurrent.STM.TChan (TChan)

data LevelThreshold = LevelThreshold {
  tMin :: Maybe Double,
  tMax :: Maybe Double
  } deriving (Show, Read)

data Threshold = Threshold {
  tHost     :: String,
  tService  :: String,
  tWarning  :: Maybe LevelThreshold,
  tCritical :: LevelThreshold
  } deriving (Show, Read)

data Metric = Metric String String String Double deriving (Show, Read)

data HimpyConfig = Hosts Integer Float String Integer String [HimpyHost] [Threshold] deriving (Show, Read)

data HimpyHost = Host String String [HimpyRecipe] deriving (Show, Read)

data HimpyRecipe = WinSrvRecipe [String] |
                   StorageRecipe |
                   LoadRecipe |
                   JuniperRecipe |
                   IostatRecipe |
                   NetworkRecipe deriving (Show, Read)

type HInternalIndex = Map (String,String) Double
type HIndex = MVar HInternalIndex

class RecipeMod mod where
  recipe :: mod -> TChan [Metric] -> TChan String -> HimpyHost -> HIndex -> IO ()
