import Data.Function
import Data.List 
import Data.Foldable (foldrM)

s :: [Int] -> Int
s (x:xs) = sum (zipWith f (x:xs) xs)
  where f x y = abs (x - y)

allA :: Int -> [Int] 
allA b = [1..b]  


-- sequence' :: [[Int]] -> [[Int]]
-- sequence' lst = foldrM (\x acc -> return (x:acc)) [] lst


sequence' [] = return []
sequence' (mx:mxs) = mx >>= \x -> 
                       sequence' mxs >>= \xs -> return (x:xs)


allAs :: [Int] -> [[Int]]
allAs bs = let as = do b <- bs  
                       return (allA b)
           in sequence' as 


findSol :: [Int] -> [([Int],Int)]
findSol blst = 
  let alist = allAs blst 
      rlist = [(a,s a) | a <- alist ]
  in sortBy (flip compare `on` snd) rlist


main = do 
  -- print (allAs [1,2,3,4])
  print (take 3 (findSol [4,2,3,4]))
