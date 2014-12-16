{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.Ratio

data MyEither a b = MyLeft a | MyRight b 
               deriving (Show)

instance Monad (MyEither a) where
  return x = MyRight x
  -- ma :: MyMaybe a, mf :: a -> MyMaybe b
  ma >>= mf = case ma of 
                MyLeft a -> MyLeft a
                MyRight a -> mf a

main = do 
  putStrLn "test''"
  flip catch (\(e :: SomeException) -> putStrLn ("some exception : " ++ show e)) $
    flip catch (\(e :: ArithException) -> putStrLn ("arith exception : " ++ show e)) $
      let x = 1 % 0 in error "xxx" >> print x 

   
  er':: Either SomeException () <- try $ do 
    er <- try (let x = 1 % 0 in print x) :: IO (Either ArithException ())
 
    case er of
      Left err -> putStrLn $ "arith exception:" ++ show err
      Right _ -> return ()
  case er' of 
    Left err -> putStrLn $ "some exception: " ++ show err
    Right _ -> return ()



  let mx = MyRight 3
      safediv x | x /= 0 = MyRight (3/x)
                | otherwise = MyLeft ("safediv",x)

      safediv2 x | x /= 1 && x/= 3 = MyRight (5 / (1-x) + 3 / (3-x))
                 | otherwise = MyLeft ("safediv2",x)
         
      y = mx >>= safediv >>= safediv2
  print y
       
