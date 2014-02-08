module System.Himpy.Config (configure) where
import System.Himpy.Types
import System.IO (readFile)

configure :: String -> IO (HimpyConfig)
configure path = do
  content <- readFile path
  return (read content :: HimpyConfig)
