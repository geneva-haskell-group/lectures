{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Binary as Bi
import qualified Data.ByteString as S 
import qualified Data.ByteString.Lazy as L

import Data.Monoid
import Data.Word 
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Network.Simple.TCP
--
import Common


main :: IO ()
main = do 
  putStrLn " I am server " 
  forkIO $ serve HostAny "5002" $ \(sock,addr) -> do 
    putStrLn $ "TCP connection established from " ++ show addr

    packAndSend sock (Message 15 "hello") 


-- ("Hello, Ian-Woo" :: S.ByteString)

  -- 

  getLine
  return ()
