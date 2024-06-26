

data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Aceitunas Int | Cebolla
                 | Jamon         | Queso
                 | Salsa         | Ricota


-- Ejercicio 1) Definir las siguientes funciones utilizando recursión estructural explícita               
-- sobre ​Pizza​: 
cantidadCapasQueCumplen   :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f  Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p') = unoSiCeroSino (f i) + cantidadCapasQueCumplen f p' 

unoSiCeroSino:: Bool -> Int 
unoSiCeroSino True = 1 
unoSiCeroSino False = 0

conCapasTransformadas  :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas f  Prepizza   =  Prepizza
conCapasTransformadas f (Capa i p') =  Capa (f i) (conCapasTransformadas f p') 

soloLasCapasQue   :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f  Prepizza   =  Prepizza 
soloLasCapasQue f (Capa i p') =  Capa ing' (soloLasCapasQue  f p' )
                                    where 
                                    ing' = sacoCapa i (f i)   

sacoCapa:: Ingrediente -> Bool ->  Ingrediente
sacoCapa i True = i 
sacoCapa _ False= Salsa

-- Ejercicio 2) Definir las siguientes funciones utilizando alguna de las definiciones                 
-- anteriores: 
sinLactosa :: Pizza -> Pizza 
sinLactosa p =  conCapasTransformadas sacaLactosa p  

sacaLactosa:: Ingrediente -> Ingrediente 
sacaLactosa Queso = Salsa 
sacaLactosa Ricota= Salsa 
sacaLactosa i     = i 

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa p = cantCapas p ==  cantidadCapasQueCumplen esApta p 
                            where esApta Queso = False 
                                  esApta Ricota = False 
                                  esApta _ = True 

cantCapas :: Pizza -> Int
cantCapas Prepizza   = 0
cantCapas (Capa i p) = 1 + cantCapas p


cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso p = cantidadCapasQueCumplen (\i-> esQueso i) p 

esQueso:: Ingrediente ->Bool 
esQueso Queso = True 
esQueso _     = False 

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas=(\p-> conCapasTransformadas dobleDeA p )

dobleDeA :: Ingrediente -> Ingrediente 
dobleDeA (Aceitunas n) = Aceitunas (2*n)
dobleDeA  x = x

-- Ejercicio 3) Definir pizzaProcesada
-- que expresa la definición de fold para la estructura de ​Pizza​
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b 
pizzaProcesada  f g Prepizza    = g  
pizzaProcesada  f g (Capa i p') =  f i (pizzaProcesada f g p')         

-- Ejercicio 4) Resolver todas las funciones de los puntos 1) y 2) utilizando la función                         
-- pizzaProcesada​.

-- a. 
cantidadCapasQueCumplen'   :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen' f = pizzaProcesada (\i acc -> unoSiCeroSino (f i) + acc) 0

--f = \i acc -> unoSiCeroSino (f i) + acc
-- ES LO MISMO QUE ESTA FUNCION:
f :: (Ingrediente -> Bool) -> Ingrediente -> Int -> Int
f condicion ingrediente acumulador = unoSiCeroSino (condicion ingrediente) + acumulador

-- b. 
conCapasTransformadas'   :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas' f = pizzaProcesada (\i p -> Capa (f i) p ) Prepizza  



-- c. 
soloLasCapasQue'  :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada 
                        (\i pp -> if f i then Capa i pp else pp ) 
                        Prepizza  


-- d 
sinLactosa' :: Pizza -> Pizza 
sinLactosa' = pizzaProcesada 
                        (\i pp -> if esQueso i then  pp else Capa i pp ) 
                        Prepizza  

aptaIntolerantesLactosa' :: Pizza -> Bool 
aptaIntolerantesLactosa' = pizzaProcesada (\i pp -> not (esQueso i) && pp )
                           True 

cantidadDeQueso' :: Pizza -> Int 
cantidadDeQueso' = pizzaProcesada (\i pp -> unoSiCeroSino (esQueso i) + pp )
                           0 

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (\i pp -> Capa (dobleDeA i) pp  )
                          Prepizza  

-- Ejercicio 5) Resolver las siguientes funciones utilizando ​pizzaProcesada (si resulta               
-- demasiado complejo resolverlas, dar primero una definición por recursión estructural                   
-- explícita, y usar la técnica de los “recuadros”): 
cantidadAceitunas :: Pizza -> Int 
cantidadAceitunas = pizzaProcesada (\i pp -> contarAceitunas i + pp  )
                        0

contarAceitunas :: Ingrediente -> Int 
contarAceitunas (Aceitunas n) = n   
contarAceitunas _             = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i pp -> if f i then i: pp  else pp ) []

conDescripcionMejorada :: Pizza -> Pizza 
conDescripcionMejorada = pizzaProcesada (\i pp -> juntarAceitunas i pp )  Prepizza

juntarAceitunas:: Ingrediente -> Pizza -> Pizza 
juntarAceitunas (Aceitunas m) (Capa (Aceitunas n) p) = Capa (Aceitunas (m+n)) p 
juntarAceitunas      i                    p        = Capa   i            p 

conCapasDe :: Pizza -> Pizza -> Pizza 
--, que agrega las capas de  la primera pizza sobre la segunda 
conCapasDe pz1 pz2 = pizzaProcesada (\i pp -> Capa i pp )  pz2 pz1  

conCapasDe':: Pizza -> Pizza -> Pizza 
conCapasDe' Prepizza pz2 = pz2 
conCapasDe' (Capa i p') pz2 = Capa i (conCapasDe' p' pz2)

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas n pz  =  (pizzaProcesada 
                    fcap
                    (\n -> Prepizza) 
                    pz)
                    n 
                    where 
                    fcap i pp 0 = Prepizza 
                    fcap i pp n = Capa i (pp (n-1))


zipPizza :: Pizza -> Pizza -> [(Ingrediente, Ingrediente)] 
zipPizza Prepizza pz2 = [] 
zipPizza pz1 Prepizza = []
zipPizza (Capa i pz1') (Capa i2 pz2') = (i,i2 ): zipPizza pz1' pz2' 


zipPizza' :: Pizza -> Pizza -> [(Ingrediente, Ingrediente)]  
zipPizza' pz1 pz2 = (pizzaProcesada 
                    g
                    (\pz2 -> []) 
                    pz1)
                    pz2 
                    where 
                    g ing1 pp Prepizza          = []
                    g ing1 pp (Capa ing2 pz2 )  = (ing1, ing2): pp pz2 



--Patron para resolver recursiones con mas casos: 
-- primerasNCapas :: Int -> Pizza -> Pizza
-- primerasNCapas n p = pizzaProcesada 
--                         (fcap 
--                         (\n -> )
--                         p
--                         n )
--                         where 
--                         fcap ing pp n = ...

primerasNCapas' :: Int -> Pizza -> Pizza
primerasNCapas' n p = (pizzaProcesada 
                        fcap 
                        (\n -> Prepizza)
                        p)
                        n 
                        where 
                        fcap i pp 0 = Prepizza 
                        fcap i pp n = Capa i (pp (n-1))


map' :: (a -> b) -> [a] -> [b]
map' f     [] = [] 
map' f (x:xs) = f x : map' f xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f     [] = []
filter' f (x:xs) =  if f x then x : filter' f xs else filter' f xs  

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z     [] = z 
foldr' f z (x:xs) = f x (foldr' f z xs) 

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f     [] = z 
recr z f (x:xs) = f x xs (recr z f xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f    [x] = x 
foldr1' f (x:xs) = f x (foldr1' f xs) 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f     [] ys =  []  
zipWith' f     xs [] =  []  
zipWith' f (x:xs) (y:ys) =  f x y : (zipWith' f xs ys)   

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f z     []=  [z]
scanr' f z (x:xs)= foldr f z (x:xs) : scanr' f z xs 


--Ejercicio 9) Definir las siguientes funciones utilizando solamente ​foldr​: 
sum :: [Int] -> Int
sum = foldr (+) 0

length :: [a] -> Int 
length = foldr (\x xs -> 1 + xs) 0

map :: (a -> b) -> [a] -> [b] 
map f = foldr (\x xs -> (f x ): xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x xs -> if (f x) then x:xs else xs) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x xs -> if (f x) then Just x  else xs) Nothing 

any :: (a -> Bool) -> [a] -> Bool 
any f = foldr (\x xs -> f x || xs) False

all :: (a -> Bool) -> [a] -> Bool 
all f = foldr (\x xs -> f x && xs) True 

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\ x xs -> if f x then 1 + xs else xs ) 0 

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\ x (xs,ys) ->if f x then (x:xs, ys) else  (xs , x:ys) ) ([] , [])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys = foldr g (\ys -> []) xs ys
                  where 
                  g  x xs     [] = [] 
                  g  x xs (y:ys) = f x y : xs ys   


takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f =  foldr (\ x xs -> if f x then x: xs else [] ) []


take :: Int -> [a] -> [a]
take n xs = foldr g (\n -> []) xs n 
            where 
            g x xs 0 = [] 
            g x xs n = x : xs (n-1)  


drop' :: Int -> [a] -> [a] 
drop' n xs = recr (\_ ->[]) g xs n 
            where 
            g x xs r 0 = x:xs  
            g x xs r n = r (n-1)  

(!!) :: Int -> [a] -> a
(!!) n xs = recr (\_ ->error "No tiene elementos") g xs n 
            where 
            g x xs r 0 = x 
            g x xs r n = r (n-1)  

-- Ejercicio 10) Indicar cuáles de las siguientes expresiones tienen tipo, y para aquellas 
-- que lo tengan, decir cuál es ese tipo: 
a. filter id 
filter :: (a -> Bool) -> [a] -> [a] 
id :: a -> a

b. map (\x y z -> (x, y, z))
map :: (a -> b) -> [a] -> [b] 

c. map (+) 
map :: (a -> b) -> [a] -> [b]
(+) :: Num a => a -> a -> a

d. filter fst 
filter :: (a -> Bool) -> [a] -> [a] 
fst :: (a,b) -> a

e. filter (flip const (+)) 
filter :: (a -> Bool) -> [a] -> [a] 
flip :: (a -> b -> c) -> b -> a -> c
const :: a -> b -> a

f. map const 
map :: (a -> b) -> [a] -> [b] 
const :: a -> b -> a
g. map twice 
map :: (a -> b) -> [a] -> [b] 
h. foldr twice 
foldr :: (a -> b -> b) -> b -> [a] -> b
i. zipWith fst 
j. foldr (\x r z -> (x, z) : r z) (const []) 
            
