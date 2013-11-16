module Himpy.Utils (timestamp) where
import qualified System.Time as T
import qualified System.Time.Utils as TU

timestamp :: IO (Integer)
timestamp = do
  t <- T.getClockTime
  let ct = T.toUTCTime t
  epoch <- TU.timelocal ct
  return epoch
