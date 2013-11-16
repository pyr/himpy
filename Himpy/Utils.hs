module Himpy.Utils (timestamp, octets) where
import Data.Bits
import Data.Word
import qualified System.Time as T
import qualified System.Time.Utils as TU

timestamp :: IO (Integer)
timestamp = do
  t <- T.getClockTime
  let ct = T.toUTCTime t
  epoch <- TU.timelocal ct
  return epoch

octets :: Word32 -> [Word8]
octets w =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]
