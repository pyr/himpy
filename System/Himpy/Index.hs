module System.Himpy.Index where
import System.Himpy.Types
import Control.Concurrent.MVar
import qualified Data.Map as M

emptyMap :: M.Map (String,String) Double
emptyMap = M.fromList []

initIndex :: IO (HIndex)
initIndex = do
  box <- newEmptyMVar
  putMVar box emptyMap
  return box

deriveMetric :: HIndex -> Integer -> (String,String) -> Double -> IO (Maybe Double)
deriveMetric box interval k metric = do
  index <- takeMVar box
  let previous = M.lookup k index
  let updated = M.insert k metric index
  putMVar box updated
  case previous of
    Nothing -> return Nothing
    Just old_metric -> return (Just ((metric - old_metric) / (fromIntegral interval :: Double)))
