import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)

{-

main :: IO ()
main = do 
  forkIO $ forever $ do 
    threadDelay 1000000
    putStrLn "chatter program"
  getLine

  return ()

-}

main :: IO ()