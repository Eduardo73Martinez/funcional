type Record a b = [(a,b)] 
-- donde la idea de este tipo es representar una fila de una base de datos (el valor de
-- tipo a es el nombre del campo, y el valor de tipo b es el valor de ese campo).
-- Con esto, puede definirse el tipo:
type Table a b = [ Record a b ] 
-- donde se entiende que una tabla de una base de datos está compuesta por muchos
-- registros, y se espera que todos compartan los mismos “campos” (o sea, los valores
-- de tipo a en cada registro).
-- Definir las siguientes funciones sin utilizar recursión estructural explícita

-- filter :: (a->Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p (x:xs) = if p x then x : filter p xs
--                          else filter p xs


--a. 
select :: (Record a b -> Bool) -> Table a b -> Table a b
-- ,
-- que a partir de la lista de registros dada describe la lista de los registros que
-- cumplen con la condición dada.
select f t= filter f t 

-- b. 
project :: (a -> Bool) -> Table a b -> Table a b
-- , que a partir
-- de la lista de registros dada describe la lista de registros solo con los campos
-- que cumplen la condición dada. 
project f =map (filter (\k -> f (fst k)))

-- c. 
conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- , que
-- describe el predicado que da True solo cuando los dos predicados dados lo
-- hacen.
conjunct f1 f2 x = f1 x && f2 x 

-- d. 
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- , que describe
-- el resultado de aplicar una función a cada elemento del producto cartesiano
-- de las dos listas de registros dadas.
crossWith f xs ys = concat (map (\x -> map (f x) ys) xs )

-- e. 
product :: Table a b -> Table a b -> Table a b
-- , que describe
-- la lista de registros resultante del producto cartesiano combinado de las dos
-- listas de registros dadas. Es decir, la unión de los campos en los registros del
-- producto cartesiano.
product table1 table2 = concatMap (\r1 -> map (\r2 -> r1 ++ r2) table2) table1


-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap f [] = []
-- concatMap f (x:xs) = f x ++ concatMap f xs

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (x:xs) = x ++ concat xs

-- f. 
similar ::(Eq a, Eq b)  => Record a b -> Record a b
-- , que describe el registro
-- resultante de descartar datos cuyos campos sean iguales (o sea, el mismo
-- dato asociado al mismo campo).
similar = foldr (\x xs-> if pertenece x xs then xs else x:xs) []

pertenece :: (Eq a, Eq b)  => (a,b) -> [(a, b)] -> Bool 
pertenece x = any (tupleElem x)

tupleElem :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
tupleElem (x1, y1) (x2, y2) = x1 == x2 && y1 == y2