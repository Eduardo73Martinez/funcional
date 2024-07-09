-- data Tree a = EmptyT
--             | NodeT a (Tree a) (Tree a)

-- foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
-- foldT z f EmptyT = z
-- foldT z f (NodeT x ti td) = f x (foldT z f ti) (foldT z f td)

-- recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
-- recT z f EmptyT = z
-- recT z f (NodeT x ti td) = f x ti td (recT z f ti) (recT z f td)

-- -- sumar "todos los elementos"
-- sumT :: Tree Int -> Int
-- sumT = foldT 0 (\x ri rd -> x + ri + rd)

-- -- contar todos los nodos
-- sizeT = foldT 0 (\x ri rd -> 1 + ri + rd)

-- -- contar cuantas hojas hay
-- countHojas :: Tree a -> Int
-- countHojas = recT 0 (\x ti td ri rd -> countSiSonEmpty ti td + ri + rd)

-- countSiSonEmpty EmptyT EmptyT = 1
-- countSiSonEmpty _ _ = 0

-- elemsDeHojas :: Tree a -> [a]
-- elemsDeHojas EmptyT = []
-- elemsDeHojas (NodeT x EmptyT EmptyT) = [x]
-- elemsDeHojas (NodeT x ti td) = elemsDeHojas ti ++ elemsDeHojas td

-- elemsDeHojas = recT z g
-- 	where z = []
-- 		  g x EmptyT EmptyT ri rd = [x]
-- 	      g x ti td ri rd = ri ++ rd

-- last :: [a] -> a
-- last []  = error "no hay ultimo si la lista es vacia"
-- last [x] = x
-- last (x:xs) = last xs

-- last = recr g z
-- 	where z = error "no hay ultimo si la lista es vacia"
-- 		  g x [] r = x
-- 	      g x xs r = r

data Dir = Left | Right | Straight

data Mapa a = Cofre [a]
            | Nada (Mapa a)
            | Bifurcacion [a] (Mapa a) (Mapa a)

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre xs) = fc xs
foldM fc fn fb (Nada m) = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs mi md) = fb xs (foldM fc fn fb mi) (foldM fc fn fb md)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b
recM fc fn fb (Cofre xs) = fc xs
recM fc fn fb (Nada m) = fn m (recM fc fn fb m)
recM fc fn fb (Bifurcacion xs mi md) = fb xs mi md (recM fc fn fb mi) (recM fc fn fb md)

-- f (Cofre xs) = ...
-- f (Nada m) = ... f m
-- f (Bifurcacion xs mi md) = ... f mi
--                            ... f md

objects :: Mapa a -> [a]
objects (Cofre xs) = xs
objects (Nada m) = objects m
objects (Bifurcacion xs mi md) = xs ++ objects mi ++ objects md

objects' = foldM (\xs -> xs) (\r -> r) (\xs ri rd -> xs ++ ri ++ rd)

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f (Cofre xs) = Cofre (map f xs)
mapM f (Nada m) = Nada (mapM f m)
mapM f (Bifurcacion xs mi md) = Bifurcacion (map f xs) (mapM f mi) (mapM f md)

mapM' f = foldM (\xs -> Cofre (map f xs)) (\r -> Nada r) (\xs ri rd -> Bifurcacion (map f xs) ri rd)

has :: (a -> Bool) -> Mapa a -> Bool
has p (Cofre xs) = any p xs
has p (Nada m) = has p m
has p (Bifurcacion xs mi md) = any p xs || has f mi || has f md

has' p = foldM (\xs -> any p xs) (\r -> r) (\xs ri rd -> any p xs || ri || rd)

-- any p (xs ++ ys) = any p xs || any p ys

-- Tarea:
-- ¿habrá alguna posibilidad de juntar mapas (perdiendo estructura) pero sin perder elementos?
-- mergeMapas :: Mapa a -> Mapa a -> Mapa a
-- y que haga válido:
-- Para p, m1, m2. has p (mergeMapas m1 m2) = has p m1 || has p m2

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt p (Cofre xs) [] = any p xs
hasObjectAt p (Nada m) [] = False
hasObjectAt p (Bifurcacion xs mi md) [] = any p xs
hasObjectAt p (Cofre xs) (d:ds) = False
hasObjectAt p (Nada m) (d:ds) = 
	case d of
		Straight -> hasObjectAt p ds
		_ -> False
hasObjectAt p (Bifurcacion xs mi md) (d:ds) =
	case d of
		Left -> hasObjectAt p mi
		Right -> hasObjectAt p md
		_ -> False

hasObjectAt p m ds = foldM fc fn fb m ds
	where fc xs [] = any p xs
		  fc xs (d:ds) = False
		  fn r (Straight:ds) = case d of
		  fn r _ = False
		  fb xs ri rd [] = any p xs
		  fb xs ri rd (Left:ds) = ri
		  fb xs ri rd (Right:ds) = rd
		  fb xs ri rd _ = False

-- zip :: [a] -> [b] -> [(a, b)]
-- zip [] ys = []
-- zip (x:xs) ys = 
--  case ys of
-- 	[] -> []
-- 	(y:ys) -> (x, y) : zip xs ys

-- zip [] ys = []
-- zip (x:xs) [] = []
-- zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- zip xs ys = foldr f z xs ys
-- 	where z ys = []
-- 	      f x r [] = []
-- 	      f x r (y:ys) = (x, y) : r ys

-- zipSobreYs x xs [] = []
-- zipSobreYs x xs (y:ys) = (x,y) : zip xs ys

-- data GenTee a = GNode a [GenTee a]

-- sumG :: GenTee Int -> Int
-- sumG (GNode x gs) = x + sumGS gs

-- sumGS :: [GenTee Int] -> Int
-- sumGS [] = 0
-- sumGS (g:gs) = sumG g + sumGS gs

-- sumG :: GenTee Int -> Int
-- sumG (GNode x gs) = x + sum (map sumG gs)

-- data Zig a = Zig a (Zag a)

-- data Zag a = Zag a (Zig a)

-- foldZig :: (a -> b -> b) -> Zig a -> b
-- foldZig f (Zip x zag) = f x (foldZag zag)

-- foldZag :: (a -> b -> b) -> Zag a -> b
-- foldZag f (Zag x zig) = f x (foldZig zig)

-- no conviene
-- hasObjectAt p (Cofre xs) [] = any p xs
-- hasObjectAt p (Bifurcacion xs mi md) [] = any p xs
-- hasObjectAt p (Nada m) (Straight:ds) = hasObjectAt p ds
-- hasObjectAt p (Bifurcacion xs mi md) (Left:ds) = hasObjectAt p mi
-- hasObjectAt p (Bifurcacion xs mi md) (Right:ds) = hasObjectAt p md
-- hasObjectAt p _ _ = False

objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel p (Cofre xs) = [xs]
objectsPerLevel p (Nada m) = [] : objectsPerLevel p m
objectsPerLevel p (Bifurcacion xs mi md) = 
	xs : juntarPorNivel (objectsPerLevel f mi) 
                        (objectsPerLevel f md)

objectsPerLevel' = foldM (\xs -> [xs]) (\r -> [] : r) (\xs ri rd -> xs : juntarPorNivel ri rd)

juntarPorNivel :: [[a]] -> [[a]] -> [[a]]
juntarPorNivel [] ys = ys
juntarPorNivel xs [] = xs
juntarPorNivel (xs:xss) (ys:yss) = xs ++ ys : juntarPorNivel xss yss

longestPath :: Mapa a -> [Dir]
longestPath = foldM (\xs -> []) (\r -> Straight : r) (\xs ri rd -> if length ri > length rd
	                                                                  then Left : ri
	                                                                  else Right : rd)

-- objectsOfLongestPath :: Mapa a -> [a]
-- objectsOfLongestPath = recM (\xs -> xs) (\m r -> r) (\xs mi md ri rd -> if heightM mi > heightM md
-- 	                                                                        then xs ++ ri
-- 	                                                                        else xs ++ rd)

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath = recM (\xs -> xs) (\m r -> r) (\xs mi md ri rd -> if length (longestPath mi) > length (longestPath md)
	                                                                        then xs ++ ri
	                                                                        else xs ++ rd)

allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (\xs -> [])
                 (\r -> agregarDir Straight r) 
                 (\xs ri rd -> agregarDir Left ri ++ agregarDir Right ri)

agregarDir d [] = [[d]]
agregarDir d dss = map (d:) dss

Para todo x, m. has (==x) m = any (elem x) (objectsPerLevel m)

-- Demostracion por inducción sobre m, siendo m un Mapa a cualquiera

Caso base m = Cofre xs

T) ¿ has (==x) (Cofre xs) = any (elem x) (objectsPerLevel (Cofre xs)) ?

-- lado izq
has (==x) (Cofre xs)
= -- def has
any (==x) xs
= -- usando propiedad demostrada en prácticas
elem x xs

-- lado der
any (elem x) (objectsPerLevel (Cofre xs))
= -- def objectsPerLevel
any (elem x) [xs]
= -- reescribo
any (elem x) (xs:[])
= -- def any
elem x xs || any (elem x) []
= -- def any
elem x xs || False
= -- prop False neutro de ||
elem x xs

Prop. ya demostrada Para x, xs. any (==x) xs = elem x xs

Otras propiedades útiles:
any (elem x) xs = elem x (concat xs)
elem x (xs ++ ys) = elem x xs || elem x ys
f (xs ++ ys) = f xs `g` f ys

Caso ind m = Nada m1

HI) has (==x) m1 = any (elem x) (objectsPerLevel m1)
TI) ¿ has (==x) (Nada m1) = any (elem x) (objectsPerLevel (Nada m1)) ?

-- lado izq
has (==x) (Nada m1)
= -- def has
has (==x) m1
= -- HI
any (elem x) (objectsPerLevel m1)

-- lado der
any (elem x) (objectsPerLevel (Nada m1))
= -- def objectsPerLevel
any (elem x) ([] : objectsPerLevel m1)
= -- def any
elem x [] || any (elem x) (objectsPerLevel m1)
= -- def elem
False || any (elem x) (objectsPerLevel m1)
= -- False nuetro de ||
any (elem x) (objectsPerLevel m1)

-- son iguales los lados

Caso ind m = Bifurcacion xs m1 m2

HI.1) has (==x) m1 = any (elem x) (objectsPerLevel m1)
HI.2) has (==x) m2 = any (elem x) (objectsPerLevel m2)
TI) ¿ has (==x) (Bifurcacion xs m1 m2) = any (elem x) (objectsPerLevel (Bifurcacion xs m1 m2)) ?

-- lado izq
has (==x) (Bifurcacion xs m1 m2)
= -- def has
any (==x) xs || has (==x) m1 || has (==x) m2
= -- HI.1, HI.2
any (==x) xs || any (elem x) (objectsPerLevel m1) || any (elem x) (objectsPerLevel m2)
= -- Prop. practica 8
elem x xs || any (elem x) (objectsPerLevel m1) || any (elem x) (objectsPerLevel m2)

-- lado der
any (elem x) (objectsPerLevel (Bifurcacion xs m1 m2))
= -- def objectsPerLevel
any (elem x) (xs : juntarPorNivel (objectsPerLevel m1) (objectsPerLevel m2))
= -- def any
elem x xs || any (elem x) (juntarPorNivel (objectsPerLevel m1) (objectsPerLevel m2))
= -- distribución según lema
elem x xs || any (elem x) (objectsPerLevel m1) || any (elem x) (objectsPerLevel m2)

------------------------------------------------------------------------------------

-- -- lema
-- Para todo xs, ys. any (elem x) xs || any (elem x) ys = any (elem x) (juntarPorNivel xs ys)

-- -- Voy a demostrar por inducción sobre xs, siendo xs una lista cualquiera

-- Caso base xs = []

-- T) ¿ any (elem x) [] || any (elem x) ys = any (elem x) (juntarPorNivel [] ys) ?

-- -- lado izq
-- any (elem x) [] || any (elem x) ys
-- = -- def any
-- False || any (elem x) ys
-- = -- False nuetro de ||
-- any (elem x) ys

-- any (elem x) (juntarPorNivel [] ys)
-- = -- def juntarPorNivel
-- any (elem x) ys

-- -- ambos lados son iguales

-- Caso ind xs = (z:zs)

-- Subcaso ys = []

-- HI) any (elem x) [] || any (elem x) ys = any (elem x) (juntarPorNivel [] ys)
-- TI) ¿ any (elem x) (z:zs) || any (elem x) [] = any (elem x) (juntarPorNivel (z:zs) []) ?

-- -- lado izq
-- any (elem x) (z:zs) || any (elem x) []
-- = -- def any
-- elem x z || any (elem x) zs || any (elem x) []
-- = -- HI
-- elem x z || any (elem x) (juntarPorNivel zs [])
-- = -- def juntarPorNivel
-- elem x z || any (elem x) zs

-- -- lado der
-- any (elem x) (juntarPorNivel (z:zs) [])
-- = -- def juntarPorNivel
-- any (elem x) (z:zs)
-- = -- def any
-- elem x z || any (elem x) zs

-- -- ambos lados iguales en subcaso ys = []

-- Subcaso ys = (w:ws)

-- HI) any (elem x) zs || any (elem x) ws = any (elem x) (juntarPorNivel zs ws)
-- TI) ¿ any (elem x) (z:zs) || any (elem x) (w:ws) = any (elem x) (juntarPorNivel (z:zs) (w:ws)) ?

-- -- lado izq
-- any (elem x) (z:zs) || any (elem x) (w:ws)
-- = -- def any
-- elem x z || any (elem x) zs || any (elem x) (w:ws)
-- = -- def any 
-- elem x z || any (elem x) zs || elem x w || any (elem x) ws
-- = -- conmutatividad
-- elem x z || elem x w || any (elem x) zs || any (elem x) ws

-- -- lado der
-- any (elem x) (juntarPorNivel (z:zs) (w:ws))
-- = -- def juntarPorNivel
-- any (elem x) (z ++ w : juntarPorNivel zs ws)
-- = -- def any
-- elem x (z ++ w) || any (elem x) (juntarPorNivel zs ws)
-- = -- HI
-- elem x (z ++ w) || any (elem x) zs || any (elem x) ws
-- = -- prop. distributiva
-- elem x z || elem x w || any (elem x) zs || any (elem x) ws

