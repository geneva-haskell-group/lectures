-- class Functor m where
--   fmap :: (a -> b) -> m a -> m b

-- class (Functor m) => Monad m where 
--   return :: a -> m a 
--  (>>=) :: m a -> ( a -> m b ) -> m b 


{-
instance Monad Maybe where 
  -- return :: a -> Maybe a 
  return x = Just x


  -- f x y = x `f` y
  -- (>>=) :: Maybe a -> ( a -> Maybe b ) -> Maybe b
  (>>=) Nothing f  = Nothing   
  (>>=) (Just x) f = f x
-}

-- Nil | Cons

{-[] | x:xs
fmap :: (a -> [b]) -> [a] -> [[b]]   

instance Monad [] where
  return x = [x]
  -- (>>=) :: [a] -> ( a -> [b] ) -> [b]
  (>>=) [] f = [] 
  (>>=) ys@(x:xs) f = concat ((f x) : (fmap f xs)) :: [b]
                        or
                    = (f x) ++ (xs >>= f)
  (>>=) ys f = concat (fmap f ys)

f mx@(Just x) = show x ++ show mx

concat :: [[b]] -> [b]

[ xs1, xs2, xs3 ] = [ [x1,x,y], [d,s,z] , [d,z,f] ] -> [ x1,x,y,d,s,z,d,z,f] 

-}

f :: Int -> Maybe String
f x | x > 0 = Just (show x)
    | otherwise = Nothing 

g :: String -> Maybe Int
g x = if length x > 3 then Nothing else Just (length x)

mx :: Maybe Int
mx = Just 323434 

mx2 :: Maybe Int
mx2 = Nothing


main :: IO ()
main = do
  print (f 3)
  print (f (-2)) 
  print (mx >>= f >>= g)



