

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
conCapasTransformadas' f = pizzaProcesada (\i p ->Capa (f i) p ) Prepizza  


-- c. 
soloLasCapasQue'  :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' = pizzaProcesada (i p ->Capa sacoCapa(i) p ) 


