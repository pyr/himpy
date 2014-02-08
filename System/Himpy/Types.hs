module System.Himpy.Types where
import Control.Concurrent.STM.TChan (TChan)

data Threshold = Threshold {
  tHost     :: String,
  tService  :: String,
  tWarning  :: Maybe Double,
  tCritical :: Double
  } deriving (Show, Read)

data Metric = Metric String String String Double deriving (Show, Read)

data HimpyConfig = Hosts String Integer [HimpyHost] [Threshold] deriving (Show, Read)

data HimpyHost = Host String String [HimpyRecipe] deriving (Show, Read)

data HimpyRecipe = WinSrvRecipe [String] |
                   StorageRecipe |
                   LoadRecipe |
                   JuniperRecipe |
                   NetworkRecipe deriving (Show, Read)

class RecipeMod mod where
  recipe :: mod -> TChan [Metric] -> TChan String -> HimpyHost -> IO ()
