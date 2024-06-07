

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
conElDobleDeAceitunas= undefined
