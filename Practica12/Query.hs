-- Una expresión del cálculo relacional de tuplas permite expresar consultas
-- sobre un modelo relacional con los siguientes componentes
type Record a b = [(a,b)] 
type Table a b = [ Record a b ] 

data Query a b = 
    Table [Record a b]           -- Table (Table a b)
    | Product (Query a b) (Query a b)
    | Projection (a -> Bool) (Query a b)
    | Selection (Record a b -> Bool) (Query a b)

-- i    :: [Record a b]  -> b 
-- Table :: [Record a b] -> Query a b 

-- h      ::           b ->          b -> b 
-- Product :: (Query a b) ->(Query a b) -> (Query a b)

-- g          ::(a -> Bool) ->          b -> b  
-- Projection ::(a -> Bool) ->(Query a b) -> (Query a b) 

-- f        :: (Record a b -> Bool) -> b           -> b 
-- Selection :: (Record a b -> Bool) -> (Query a b) -> (Query a b)

-- Este modelo es utilizado para estudiar y manipular las consultas hechas a motores
-- de bases de datos. En este contexto vemos a cada registro como una fila y a cada
-- elemento como un par columna-valor. Por ejemplo:

--a. Dar el tipo y definir la función foldQ, que expresa el esquema de recursión
--estructural para la estructura Query.
foldQ :: ((Record a b -> Bool) -> b -> b ) -> ((a -> Bool) -> b -> b) -> ( b -> b -> b ) -> ([Record a b]-> b) -> (Query a b) -> b
foldQ f g h i       (Table xs) = i xs 
foldQ f g h i   (Product q1 q2)= h (foldQ f g h i q1) (foldQ f g h i q2) 
foldQ f g h i (Projection f' q)= g f' (foldQ f g h i q) 
foldQ f g h i  (Selection f' q)= f f' (foldQ f g h i q) 

--a. Definir las siguientes funciones sin utilizar recursión explícita:

--i. 
{-- 
tables :: Query a b -> [Table a b]
--, que describe la lista de
--todas las tablas involucradas en la query dada.
--tables = foldQ (\_ xs -> xs) (\_ xs -> xs) (\xs ys -> xs ys)  (\t -> [t]) 
tables query = foldQ (\_ tables -> tables)   -- processSelection
                     (\_ tables -> tables)   -- processProjection
                     (\tables1 tables2 -> tables1 ++ tables2)  -- processProduct
                     (\records -> [records])  -- processTable
                     query
--}



tables' :: Query a b -> [Table a b]
--, que describe la lista de
--todas las tablas involucradas en la query dada.
tables'        (Table xs)=  [xs] 
tables'   (Product q1 q2)= tables' q1 ++ tables' q2 
tables' (Projection f' q)= tables' q 
tables'  (Selection f' q)= tables' q 

--ii. 
execute :: Query a b -> Table a b
-- , que describe el
--resultado de ejecutar la query dada
execute        (Table xs)=   xs 
execute   (Product q1 q2)= products (execute q1) (execute q2) 
execute (Projection f' q)= projection f' (execute q) 
execute  (Selection f' q)= selection f'  (execute q) 


products ::Table a b-> Table a b-> Table a b 
products t1 t2 = concatMap (\r1 -> map (r1 ++) t2) t1

projection:: (a->Bool) -> Table a b-> Table a b  
projection  f table = map (\record -> filter (\(a, _) -> f a) record) table

selection:: (Record a b->Bool) -> Table a b-> Table a b 
selection f t = filter f t


-- iii. 
compact :: Query a b -> Query a b 
--, que describe la query
-- resultante de compactar las selecciones y proyecciones consecutivas
-- en la query dada.
compact = undefined 

-- b. Demostrar la siguiente propiedad:
-- execute . compact = execute