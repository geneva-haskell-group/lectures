import Control.Concurrent
import Control.Monad (forever, replicateM_ )
-- import Data.IORef
import Control.Concurrent.STM

-- TVar a 
-- STM r

-- atomically :: STM a -> IO a 

threadA :: (TVar Int, TVar Int) -> IO ()
threadA (aref,bref) = do
  (a,b) <- atomically $ do 
     a <- readTVar aref
     b <- readTVar bref  
     if b < 5 then retry else do  
       writeTVar aref (a+1) -- a <- a +1 
       writeTVar bref (b-1)
     return (a,b)
  putStrLn   $ "I am threadA: " ++ show (a,b)
  threadDelay 200

threadB :: (TVar Int, TVar Int) -> IO ()
threadB (aref,bref) = do
  (a,b) <- atomically $ do 
    a <- readTVar aref 
    b <- readTVar bref 
    writeTVar aref (a-1)
    writeTVar bref (b+1)
    return (a,b) 
  putStrLn $ "I am threadB: " ++ show (a,b)
  threadDelay 1000


main = do 
  putStrLn "testing IORef"

  refs <- atomically $ do 
            aref <- newTVar 10
            bref <- newTVar 10
            return (aref,bref)


 
  forkIO $ replicateM_ 100 (threadA refs)

  replicateM_ 100 (threadB refs)

