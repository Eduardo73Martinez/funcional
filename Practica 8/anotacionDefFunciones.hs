any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) = p x || any p xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs 

elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem y (x:xs) = y == x || elem y xs

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True   -- El conjunto vacío es un subconjunto de cualquier conjunto
subset (x:xs) ys = elem x ys && subset xs ys

all :: (a -> Bool) -> [a] -> Bool
all _ []     = True   -- Lista vacía cumple la condición trivialmente
all p (x:xs) = p x && all p xs


lado izq ) 
    
lado der ) 
