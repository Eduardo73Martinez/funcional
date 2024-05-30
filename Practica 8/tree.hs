data Tree a = EmptyT 
            | NodeT a (Tree a) (Tree a)

f EmptyT = ...
f (NodeT x ti td) =
	... f ti
	... f td

heightT :: Tree a -> Int
heightT EmptyT = ...
heightT (NodeT x ti td) =
	... heightT ti
	... heightT td

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = ...
listPerLevel (NodeT x ti td) =
	... listPerLevel ti
	... listPerLevel td

levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT = ...
levelN 0 (NodeT x ti td) = ...
levelN n EmptyT = ...
levelN n (NodeT x ti td) =
	... levelN n ti
	... levelN n td

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = ...
ramaMasLarga (NodeT x ti td) =
	... ramaMasLarga ti
	... ramaMasLarga td

-- todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = ...
todosLosCaminos (NodeT x ti td) =
	... todosLosCaminos ti
	... todosLosCaminos td

¿ heightT = length . ramaMasLarga ?

-- Sugerencia:

-- lema
-- se demuestra por caso
-- tarea
-- Para todo f, x, y. 
--   f (if b then x else y) = if b then f x else f y

-- y no dividir por casos