{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO

main :: IO ()
main = do 
  putStrLn "hello"

  tchan :: TChan String <- atomically $ newBroadcastTChan

  forkIO $ client 1 tchan
  forkIO $ client 2 tchan
  forkIO $ client 3 tchan


  forever $ do 
    msg <- getLine
    atomically $ writeTChan tchan msg

  return ()



client :: Int -> TChan String -> IO ()
client n bchan = do 
    chan <- atomically $ dupTChan bchan
    forever $ do 
      msg <- atomically $ readTChan chan 
      putStrLn $ "client " ++ show n ++ ":" ++msg
