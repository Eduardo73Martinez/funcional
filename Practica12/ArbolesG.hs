
--Ejercicio 7) Dada la siguiente definición para representar árboles generales:
data GTree a = GNode a [GTree a] deriving Show
-- a. Dar tipo y definir las tres versiones de foldGT y recGT, que expresan los
-- esquemas de recursión estructural y primitiva, respectivamente, para la
-- estructura GTree.
-- ATENCIÓN: recordar que no siguen la secuencia dada para tipos recurslvos
-- con recursión directa, porque la recursión en GTree NO es directa. Estas
-- versiones deben tener en cuenta el esquema de recursión sobre listas
-- correspondiente, como se trató en clase teórica.

-- f     :: a -> [b] -> b 
-- GNode :: a -> [GTree a] -> GTree a 

-- r :: b -> b 
-- resultado:: GTree a -> GTree a 

foldGT0 :: (a -> [b] -> b)  ->  GTree a -> b 
foldGT0 f  (GNode x xs ) = f x (map (foldGT0 f) xs)  

--completamente estructural 
foldGT1 :: (a->c->b) -> (b->c->c) -> c -> GTree a -> b
foldGT1 g f z (GNode x ts) = g x (foldr f z (map (foldGT1 g f z) ts))

-- solo estructural si k es estructural.
foldGT :: (a->c->b) -> ([b]->c) -> GTree a -> b
foldGT g k (GNode x ts) = g x (k (map (foldGT g k) ts))

ejemploArbol :: GTree Int
ejemploArbol =
    GNode 1 [
        GNode 2 [
            GNode 4 [],
            GNode 5 []
        ],
        GNode 3 [
            GNode 6 [],
            GNode 7 [],
            GNode 8 []
        ],
        GNode 9 [],
        GNode 10 []
    ]



--b. Definir las siguientes funciones sin utilizar recursión explícita:
--i. 
mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f (GNode x xs) = GNode (f x) (map (mapGT f) xs)

--xs == [(GNode x1 xs1),(GNode x2 ys1),(GNode x3 ys2), (GNode x4 xs2),(GNode x5 xs3) ]

--ii. 
sumGT :: GTree Int -> Int
sumGT (GNode x xs)= x + sum (map sumGT xs)

--iii. 
sizeGT :: GTree a -> Int
sizeGT (GNode x xs)= 1 + length (map sizeGT xs) 

--iv. 
heightGT :: GTree a -> Int
heightGT (GNode _ []) = 1  -- Altura de un nodo sin hijos es 1
heightGT (GNode _ xs) = 1 + maximum (map heightGT xs)

--v. 
preOrderGT :: GTree a -> [a]
preOrderGT (GNode x xs) = x : concatMap preOrderGT xs

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap f [] = []
-- concatMap f (x:xs) = f x ++ concatMap f xs


--vi. 
postOrderGT :: GTree a -> [a]
postOrderGT (GNode x xs) = (concatMap preOrderGT xs) ++ [x]

--vii. 
mirrorGT :: GTree a -> GTree a
mirrorGT (GNode x xs)= GNode  x (reverse (map mirrorGT xs))  

--viii. 
countByGT :: (a -> Bool) -> GTree a -> Int
countByGT f (GNode x xs )=  if f x
    then 1 + foldr (\subtree acc -> acc + countByGT f subtree) 0 xs
    else foldr (\subtree acc -> acc + countByGT f subtree) 0 xs

--ix. 
partitionGT:: (a -> Bool) -> GTree a -> ([a], [a])
partitionGT = undefined 

--x. 
zipWithGT:: (a->b->c) -> GTree a -> GTree b -> GTree c
zipWithGT = undefined 

--xi. 
caminoMasLargoGT :: GTree a -> [a]
caminoMasLargoGT = undefined 

--xii. 
todosLosCaminosGT :: GTree a -> [[a]]
todosLosCaminosGT = undefined 

--xiii. 
todosLosNivelesGT :: GTree a -> [[a]]
todosLosNivelesGT = undefined 

--xiv. 
caminoHastaGT :: Eq a => a -> GTree a -> [a]
caminoHastaGT = undefined 

--xv. 
nivelNGT :: GTree a -> Int -> [a]
nivelNGT = undefined 