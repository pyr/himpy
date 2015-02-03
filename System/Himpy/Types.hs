module System.Himpy.Types where
import Control.Concurrent.MVar (MVar)
import Data.Map (Map)
import Control.Concurrent.STM.TChan (TChan)

data Threshold = Threshold {
  tHost     :: String,
  tService  :: String,
  tWarning  :: Maybe Double,
  tCritical :: Double
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
  recipe :: mod -> TChan [Metric] -> TChan String -> Integer -> HimpyHost -> HIndex -> IO ()
