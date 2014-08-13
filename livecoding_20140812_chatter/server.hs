{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Binary as Bi
import qualified Data.ByteString as S 
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Monoid
import Data.Word 
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Network.Simple.TCP
--
import Common




main :: IO ()
main = do 
  putStrLn " I am server " 

  tvar <- atomically $ newTVar ([] :: [Message] )

  tvarLog <- atomically $ newTVar ([] :: [Message])

  forkIO $ serve HostAny "5002" $ \(sock,addr) -> do 
    putStrLn $ "TCP connection established from " ++ show addr
    forever $ do 
      nmsgs <- atomically $ do 
                 bufmsgs <-readTVar tvar
                 if null bufmsgs 
                   then retry 
                   else writeTVar tvar []
                 return bufmsgs
      newlogs <- atomically $ do 
        logs <- readTVar tvarLog
        let newlogs = nmsgs ++ logs
        writeTVar tvarLog newlogs 
        return newlogs
      print newlogs
      packAndSend sock newlogs -- (Message n "hello") 

  serve HostAny "5003" $ \(sock,addr) -> forever $ do 
    putStrLn $ "Getting message from " ++ show addr
    mtxt :: Maybe Message <- recvAndUnpack sock
    case mtxt of 
      Nothing -> return () 
      Just txt -> do putStrLn $ "got message : " ++ show txt
                     atomically $ do 
                       txts <- readTVar tvar
                       writeTVar tvar (txt:txts)
   
{-
  forever $ do
    threadDelay 1000000  
    atomically $ do n <- readTVar tvar
                    writeTVar tvar (n+1) 
-}


-- ("Hello, Ian-Woo" :: S.ByteString)

  -- 

  getLine
  return ()
