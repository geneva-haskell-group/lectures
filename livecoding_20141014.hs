import Debug.Trace

x :: Integer
x = 4

y = (1,2,"string") 



z = [1,2,3,4]

--  single line
{-  multiple 
     line comment -}  
-- type signature 
f :: Integer -> String 
f x = show (x + 1)

-- show :: (Show a) => a -> String 

g :: Integer -> Integer 
g x = let y = x + z 
          z = x * y
      in y * y + z 

g2 x = y * y + z
  where y = x + 1
        z = x * x  

-- h x | x > 0 = y 
--     | x < 0 = 1
--   where y = x*x

-- lst = [1,2,3] 

-- empty  | non-empty
-- []     | (x : xs)

-- [1,2] = 1 : ( 2 : []  ) 

-- : = cons

-- [1,2,3] = 1 : (2 : ( 3 : [] ) )

-- lst = 1 : lst' 
--  where lst' = 


plus x = x + 1
 
--- take 5 lst 

-- [a] = [] | (x : xs)

-- map :: (a->b) -> [a] -> [b]
-- map f []     = []
-- map f (x:xs) = f x : map' f xs  

-- take' :: Int -> [a] -> [a] 
-- take' n []                 = [] 
-- take' n (x:xs) | n > 0     = x: (take' (n-1) xs)
--                | otherwise = [] 

-- built-in function => Prelude  
-- data Bool = True | False

filter' :: ( a -> Bool ) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x 
                     then x : (filter' p xs)  
                     else filter' p xs

-- filter' p (x:xs) 
--   | p x = x : (filter' p xs)  
--   | otherwise = filter' p xs


nat :: [Integer]
nat = 1 : (map plus nat)

nat2 :: [Integer]
nat2 = tail nat   -- == 2 : (map plus nat2) 
      
primes :: [Integer]
primes = sieve nat2
  where sieve :: [Integer] -> [Integer]
        sieve [] = [] 
        -- correct implementation
        sieve (x:xs) = x: sieve (filter (\y -> y `mod` x /= 0) xs)
        
        -- wrong implementation
        --sieve (x:xs) = trace (show (take 20 xs)) 
        --                 (x:(filter (\y -> y `mod` x /= 0) (sieve xs)))
       
        -- some note on wrong implementation

--         -- run1
--         sieve (2:[3,4,5,6...]) = 2:(filterout 2 (sieve [3,4,5,6...])
--          = 2:(filterout 2 (unevaluated) -- 3:filterout 3 (sieve [4,5,6,7...])
--          = 2:(filterout 2 (3:filterout 3 (4: filter 4 (sieve [5,6,7,8... )  


--    (filterout 2 (3:filterout 3 (4: filter 4 (sieve [5,6,7,8... )

-- thunk = unevaluated expression
-- ------------------------------
--     = filterout 2 (3: thunk)
--     =  3 : ( filterout 2 thunk ) 
       


--    take 5 prime = 2 : 3 : (filterout 2 : filterout 3 : (4 :... )..

--         -- run2 
--         sieve (3:[4,5,6,7...]) = ...  (sieve [4,5,6,7...])
--         -- run3
--         sieve (4:[5,6,7,8...]) = ...


