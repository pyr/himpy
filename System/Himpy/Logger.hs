module System.Himpy.Logger (log_start, log_info) where
import System.IO (openFile, hPutStrLn, hFlush, hClose,
                  IOMode (AppendMode), Handle)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan, readTChan, newTChanIO, TChan)

-- Simplistic logging module

log_line :: TChan String -> String -> IO ()
log_line chan path = do
  line <- atomically $ readTChan chan
  fd <- openFile path AppendMode
  hPutStrLn fd line
  hFlush fd
  hClose fd

log_start :: String -> IO (TChan String)
log_start path = do
  chan <- newTChanIO
  void $ forkIO $ forever $ log_line chan path
  return (chan)

log_info :: TChan String -> String -> IO ()
log_info chan msg = do
  let info_msg = "[info] " ++ msg
  atomically $ writeTChan chan info_msg
