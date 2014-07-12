{-# LANGUAGE DeriveFunctor #-}

-- functor 

-- class Functor f where 
--   fmap ::( a -> b) -> f a -> f b


-- [a] = [] a -- f = [] a = a 

-- instance Functor [] where 
--   fmap = map 

-- map :: (a->b) -> [a] -> [b]

-- data Maybe a = Nothing | Just a

-- instance Functor Maybe where
--   -- fmap :: (a->b) -> Maybe a -> Maybe b
--   fmap f Nothing = Nothing
--   fmap f (Just x) = Just (f x)


data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Functor) 

fmap' f (Leaf x) = Leaf (f x)
fmap' f (Node l r) = Node (fmap f l) (fmap f r)

-- class Show a where
--   show :: a -> String  

-- instance Show (Tree a) where
--   show 

testtree :: Tree Int
testtree = Node (Node (Leaf 3) (Leaf 4)) (Node (Leaf 5) (Leaf 7)) 

func :: Int -> String
func x = show x ++ "!"

main :: IO () 
main = do 
  let lst= [1,2,3,4] :: [Int]
  print (fmap (\x->x+1) lst) 

  let may = Just 1 :: Maybe Int
  print (fmap (\x->x+1) may)

  print testtree
  print (func 3) 
  print (fmap func testtree)

  print (fmap' func testtree)