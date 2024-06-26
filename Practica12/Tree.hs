--Ejercicio 3) Dada la definición de Tree:
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
    deriving Show
--a. Dar el tipo y definir la función foldT, que expresa el esquema de recursión
--estructural para la estructura Tree.

-- g      :: b 
-- EmptyT :: Tree a 
-- f     :: a ->        b ->       b    ->   b 
-- NodeT :: a -> (Tree a) -> (Tree a)   ->   (Tree a) 

-- resultado = Tree a -> b  

foldT ::(a -> b -> b -> b ) -> b -> Tree a -> b  
foldT f z          EmptyT = z  
foldT f z (NodeT x ti td) = f x (foldT f z ti) (foldT f z td ) 

--sb. Definir las siguientes funciones utilizando foldT:

--i. 
-- mapT :: (a -> b) -> Tree a -> Tree b 
-- mapT f          EmptyT = EmptyT 
-- mapT f (NodeT x ti td) = NodeT (f x) (mapT f ti)  (mapT f td) 

mapT :: (a -> b) -> Tree a -> Tree b 
mapT f = foldT (\x ti td ->NodeT (f x) ti td ) EmptyT      

miF :: Int -> Int 
miF x = x - 3

--ii. 
sumT :: Tree Int -> Int
sumT = foldT (\x ti td ->x + ti + td ) 0 

--iii. 
sizeT :: Tree a -> Int
sizeT = foldT (\x ti td ->1 + ti + td ) 0 

--iv. 
heightT :: Tree a -> Int
heightT = foldT (\x ti td ->1 + max ti  td ) 0

--v. 
preOrder :: Tree a -> [a]
preOrder = foldT (\x ti td ->[x] ++ ti ++ td ) []

--vi. 
inOrder :: Tree a -> [a]
inOrder = foldT (\x ti td -> ti ++ [x] ++ td ) []

--vii. 
postOrder :: Tree a -> [a]
postOrder = foldT (\x ti td -> ti ++ td ++ [x] ) []

--viii. 
mirrorT :: Tree a -> Tree a
mirrorT = foldT (\x ti td ->NodeT x td ti ) EmptyT 

--ix. 
countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT (\x ti td ->if f x then 1 + ti + td else ti + td ) 0 

--x. 
-- partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
-- partitionT f = foldT (\x (xs, ys) -> if f x then (x:xs, ys) else (xs, x:ys)) ([], []) 

-- zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
-- zipWithT _ EmptyT _ = EmptyT
-- zipWithT _ _ EmptyT = EmptyT
-- zipWithT f (NodeT x left1 right1) (NodeT y left2 right2) =

--     NodeT (f x y) (zipWithT f left1 left2) (zipWithT f right1 right2)
--xi. 
zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f t1 t2 = foldT zipFunc EmptyT t1
  where
    zipFunc x leftResult rightResult = case t2 of
        EmptyT -> EmptyT  -- Si t2 es EmptyT, no hay elementos correspondientes para combinar
        NodeT y left2 right2 -> NodeT (f x y) leftResult rightResult

                                    
                                            

--xii. 
caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT (\x ti td -> if length ti > length td  then [x] ++ ti else [x] ++ td  ) []

--xiii. 
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos =  foldT (\x ti td -> [x] : consACada x ti ++ consACada x td ) []

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss


--xiv. 
todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT (\x ti td -> [x] : g ti td  ) []
                        where 
                        g [] yss = yss
                        g xss [] = xss
                        g (x:xss) (y:yss) = (x++y): g xss yss 

-- listaEntrePrimeros:: [[a]]->[[a]] ->[[a]] 
-- listaEntrePrimeros []       yss   = yss
-- listaEntrePrimeros xss       []   = xss
-- listaEntrePrimeros (x:xss) (y:yss) = (x++y): listaEntrePrimeros xss yss


levelN :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
--Nota: El primer nivel de un árbol (su raíz) es 0.
levelN _ EmptyT          = []                                   -- caso base del arbol 
levelN 0 (NodeT x _  _ ) = x : []                               --1er caso recursivo del árbol 
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2   --2do caso recursivo del árbol

--xv. 
nivelN :: Tree a -> Int -> [a]
nivelN tree n = foldT (\x left _ ->g n left x ) [] tree
                     where 
                    g 0 _ x = x : []
                    g n t x = g (n-1) t x 


-- c. Dar el tipo y definir la función recT, que expresa el esquema de recursión
-- primitiva para la estructura Tree.

-- g      :: b 
-- EmptyT :: Tree a 
-- f     :: a ->        b ->       b    ->   b 
-- NodeT :: a -> (Tree a) -> (Tree a)   ->   (Tree a) 

-- resultado = Tree a -> b   

recT :: b -> (a -> Tree a -> Tree a ->b -> b -> b) -> Tree a -> b
recT g f          EmptyT = g 
recT g f (NodeT x ti td) = f x ti td (recT g f ti)  (recT g f td)

inserT ::Ord a => a -> Tree a -> Tree a
-- PRECONDICION: NO PUEDE ESTAR EN EL ARBOL DADO! 
inserT x          EmptyT = NodeT x EmptyT EmptyT
inserT x (NodeT y ti td) =  if x < y 
                            then NodeT y (inserT x ti) td  
                            else NodeT y ti  (inserT x td) 

insertT' :: Ord a => a -> Tree a -> Tree a
--que describe el arbol 
-- resultante de insertar el elemento dado en el árbol dado, teniendo en
-- cuenta invariantes de BST.
insertT' x t = recT (\x t -> NodeT x EmptyT EmptyT ) (\x ti td -> NodeT x ti td ) 

caminoHasta :: Eq a => a -> Tree a -> [a]
--, que describe
-- el camino hasta el elemento dado en el árbol dado.
-- Precondición: existe el elemento en el árbol
caminoHasta = undefined


-- Árbol BST con 6 elementos
ejemploBST :: Tree Int
ejemploBST =
    NodeT 3
        (NodeT 2
            (NodeT 1 EmptyT EmptyT)
            EmptyT)
        (NodeT 4
            EmptyT
            (NodeT 5 EmptyT EmptyT))
