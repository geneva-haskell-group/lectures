
import Control.Applicative
import Control.Monad.State (State,runState, get,put)

import Data.List (foldl')
-- import Data.Maybe (mapMaybe)
import qualified Data.Traversable as T (sequenceA, sequence)

myid :: [a] -> [a]
myid xs = foldr (:) [] xs 

myreverse :: [a] -> [a] 
myreverse xs = (foldr (\x acc -> acc . (x:)) id xs) [] 

catMaybes :: [Maybe a] -> [a] 
catMaybes mxs = foldr f [] mxs 
  where f (Just x) acc = x:acc 
        f Nothing acc = acc 

mapMaybe :: (a -> Maybe b) -> [a] -> [b] 
mapMaybe f = catMaybes . map f

f lst@(x:xs) = zipWith (+) xs lst 

fib = 0:1:f fib

for = flip map

m = [ [ 1, 2, 3 ] 
    , [ 4, 5, 6 ]  
    , [ 7, 8, 9 ] ]

m2 = [ [ 1,2,3]
     , [ 1,2,3,4]
     , [ 1,2] ] 

m3 = [ [ 1,0,0] 
     , [ 0,1,0]
     , [ 0,0,1] ]

-- dot v w = foldl' (+) 0 . map (uncurry (*)) $ zip v w
dot v w = sum (zipWith (*) v w) 

trans :: [[a]] -> [[a]] 
trans [] = []
trans [[]] = []
trans lst = 
    let safehead [] = Nothing
        safehead (x:xs) = Just x
        safetail [] = Nothing
        safetail (x:xs) = Just xs 
        lst' = mapMaybe safetail lst
        x' = mapMaybe safehead lst 
    in case x' of 
         [] -> [] 
         _ -> x' : trans lst' 


mmult1 :: [[Int]] -> [[Int]] -> [[Int]] 
mmult1 = -- for vs $ \v -> 
               --  for ws $ \w -> v `dot` w
               tensorBy dot 

mmult vs ws = (mmult1 vs . trans) ws
-- mmult vs ws= v 


-- trans :: [[a]] -> [[a]] 
-- trans = foldr ( \x acc ->  




tensorBy :: (a -> b -> c) -> [a] -> [b] -> [[c]] 
tensorBy f xs ys = for xs $ \x -> for ys $ \y -> f x y



{-
checkMaybes :: [Maybe x] -> Maybe [x]
-- checkMaybes [] = Just [] 
-- checkMaybes (mx:mxs) = (:) <$> mx <*> checkMaybes mxs 

checkMaybes = genfunc (:) 

genfunc :: (x->[x]->[x]) -> [Maybe x] -> Maybe [x]
genfunc f [] = Just [] 
genfunc f (mx:mxs) = f <$> mx <*> genfunc f mxs 

genfunc2 :: (x->[x]->Maybe [x]) -> [Maybe x] -> Maybe [x]
genfunc2 mf [] = Just [] 
genfunc2 mf (mx:mxs) = do x <- mx 
                          xs <- genfunc2 mf mxs 
                          mf x xs 

-- mf <*> mx =<< genfunc mf mxs 



foldrM :: Monad m => (a -> b -> m b) -> b -> [m a] -> [m b]
foldrM mf acc [] = pure acc
foldrM mf acc (mx:mxs) = mf <*> mx <*> foldrM mf acc mxs
-}

checkMaybes :: [Maybe x] -> Maybe [x]
checkMaybes [] = Just []
checkMaybes (mx:mxs) = 
    case mx of 
      Nothing -> Nothing
      Just x -> 
        case checkMaybes mxs of 
          Nothing -> Nothing
          Just xs -> Just (x:xs)

checkMaybes' :: [Maybe a] -> Maybe [a]
checkMaybes' [] = return []
checkMaybes' (mx:mxs) = mx >>= 
                          (\x -> checkMaybes' mxs >>= (\xs -> return (x:xs)))



linePrint :: (Show a) => [a] -> IO ()
linePrint xs = sequence (map print xs) >> return ()
  

mymapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mymapM mf xs = sequence (map mf xs)


put1more :: State Int ()
put1more = do st <- get
              put (st+1)


main = do 

  print $ take 100 fib

  print (map (\x->map (*x) [0..5]) [0..10])

  print $ for [0..10] $ \x ->
            for [0..5] $ \y -> x*y

  print $ trans ([[]] :: [[Int]])

  print $ trans m 

  print $ trans ([[],[],[]] :: [[Int]])

  print $ trans m2 

  let v1 = [1,2,3]
      v2 = [4,5,6] 
  print $ dot v1 v2

  print $ m `mmult` m3


  print $ myid [1,2,3]  

  print $ myreverse [1,2,3]

  print $ catMaybes [ Just 1, Just 3, Nothing, Just 4]


  print $ tensorBy (*) [1,2,3] [4,5,6]
  print $ tensorBy (+) [1,2,3] [4,5,6]
  print $ tensorBy (,) [1,2,3] [4,5,6]

  print (checkMaybes [Just 1, Just 2])
  print (checkMaybes [Just 1, Nothing])
 

  print (checkMaybes' [Just 1, Just 2])

  print (sequence [Just 1, Just 2]) 

  sequence [print 1, print 2]
  linePrint m 

  print (runState (mymapM (\_ -> put1more >> return ()) [1,2,3,4,5]) 0)
