data Multiset a = MS (a -> Int)

-- denota un multiset vacío
-- (no contiene apariciones de ningún elemento)
emptyMS :: Multiset a
emptyMS = MS (\x -> 0)

-- indica la cantidad de apariciones
count :: Eq a => a -> Multiset a -> Int
count x (MS f) = f(x)

-- incrementa una aparición
add :: Eq a => a -> Multiset a -> Multiset a
add x (MS f) = MS (\y -> if x == y then f y + 1 else f y)

m1 = emptyMS 
m2 = add 1 (m1)

m3 = add 1 (m2)  
m4 = add 1 (m3) 
m5 = add 1 (m4) 


-- decrementa una aparición
remove :: Eq a => a -> Multiset a -> Multiset a
remove x (MS f)= MS (\y -> if x == y then f y -1 else f y) 

-- junta dos multiset
-- donde las apariciones para cada elemento
-- son sumadas
union :: Multiset a -> Multiset a -> Multiset a
union (MS f) (MS g)= MS (\x -> f(x) + g(x)) 
-- junta dos multiset
-- donde las apariciones para cada elemento
-- corresponde a la menor cantidad de apariciones
-- entre ellos
intersection :: Multiset a -> Multiset a -> Multiset a
intersection (MS f1) (MS f2) = MS (\x -> if f1 x <= f2 x then f1 x else f2 x)


intersection' :: Multiset a -> Multiset a -> Multiset a
--Mas lindo 
intersection' (MS f1) (MS f2) = MS (\x -> min (f1 x) (f2 x))
