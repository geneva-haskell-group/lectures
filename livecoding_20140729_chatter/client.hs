{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Binary as Bi
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Network.Simple.TCP
--
import Common


main :: IO ()
main = do 
  putStrLn " i am client " 

  connect "127.0.0.1" "5002" $ \(sock,addr) -> do 
    putStrLn $ "Connection established to " ++ show addr
    x  :: Maybe Message <- recvAndUnpack sock 
    print x      
