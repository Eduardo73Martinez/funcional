-- Ejercicio N°1 
data Pizza = Prepizza | Capa Ingrediente Pizza 
data Ingrediente = Aceitunas Int | Cebolla
                 | Jamon         | Queso
                 | Salsa         | Ricota

-- CASO BASE 
-- Prepizza esta en el conjunto Pizza  

-- CASO INDUCTIVO 
-- * Sea i un elemento del conjunto Ingrediente
--   y p un elemento del conjunto Pizza,
--   entonces Capa i p está en el conjunto Pizza

--Eje 2
-- esquema de recursión estructural 
-- sobre... Pizza
-- f Prepizza = ...
-- f (Capa i p) = ... f p 

--EJE 3
cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas (Prepizza) = 0 
cantidadDeCapas (Capa i p2) = 1 + cantidadDeCapas p2  


cantidadDeAceitunas:: Pizza -> Int 
cantidadDeAceitunas (Prepizza) = 0 
cantidadDeAceitunas (Capa i p) = cantidadAceituna' i + cantidadDeAceitunas p 

cantidadAceituna':: Ingrediente -> Int 
cantidadAceituna' (Aceitunas cantidad) = cantidad 
cantidadAceituna' _ = 0


duplicarAceitunas:: Pizza -> Pizza 
duplicarAceitunas (Prepizza) = Prepizza 
duplicarAceitunas (Capa i p) =  Capa (duplicarSiHay i) ( duplicarAceitunas p )


duplicarSiHay:: Ingrediente -> Ingrediente 
duplicarSiHay (Aceitunas i) = Aceitunas (2*i)  
duplicarSiHay x             = x 

sinLactosa:: Pizza ->Pizza 
sinLactosa (Prepizza) = Prepizza 
sinLactosa (Capa i p) = agregarSinLactosa i (sinLactosa p )

agregarSinLactosa :: Ingrediente -> Pizza -> Pizza 
agregarSinLactosa i p = if contieneLactosa i 
                            then p 
                            else Capa i p 

contieneLactosa :: Ingrediente -> Bool 
contieneLactosa Queso   = True 
contieneLactosa Ricota  = True 
contieneLactosa _       = False 


aptaIntolerantesLactosa:: Pizza -> Bool 
aptaIntolerantesLactosa (Prepizza) = True  
aptaIntolerantesLactosa (Capa i p) =  not (contieneLactosa i)  && (aptaIntolerantesLactosa p) 

conAceitunasJuntas :: Pizza -> Pizza 
conAceitunasJuntas (Prepizza) = Prepizza 
conAceitunasJuntas (Capa i p) = juntarAceitunas i  (conAceitunasJuntas p) 

juntarAceitunas:: Ingrediente -> Pizza -> Pizza 
juntarAceitunas (Aceitunas m) (Capa (Aceitunas n) p) = Capa (Aceitunas (m+n)) p 
juntarAceitunas      i                    p        = Capa   i            p 


----------------------------------------------------------------------------------------------
--                                  SECCIÓN 2 
type Nombre = String 
data Planilla = Fin | Registro Nombre Planilla 
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo


--CASO BASE (Planilla) 
-- FIN ESTÁ EN EL CONJUNTO PLANILLA 

--CASO INDUCTIVO (planilla)
--SEA N UN ELEMENTO QUE PERTENECE A NOMBRE Y P UN ELEMENTO QUE PERTENECE A PLANILLA 
-- ENTONCES Registro N P  PERTENECE A PLANILLA.

-- ESQUEMA DE RECURSION (planilla)
-- f (Fin) = ...
-- f (Registro n p) =  ... n ... f(p)

--CASO BASE (Equipo)
-- SI N ESTÁ EN EL CONJUNTO NOMBRE, ENTONCES BECARIO N ESTA EN EL CONJUNTO EQUIPO

--CASO INDUCTIVO (Equipo)
-- SEA N UN ELEMENTO QUE PERTENECE AL CONJUNTO NOMBRE,
-- Y E1 ESTA EN EL CONJUNTO EQUIPO 
-- Y E2 ESTÁ EN EL CONJUNTO EQUIPO 
-- Y E3 ESTÁ EN EL CONJUNTO EQUIPO, 
-- ENTONCES INVESTIGADOR N E1 E2 E3 ESTÁ EN EL CONJUNTO EQUIPO

-- ESQUEMA DE RECURSION (Equipo)
-- f (Becario n) = ...
-- f (Investigador n e1 e2 e3) =
--     ... f e1
--     ... f e2
--     ... f e3


--EJE N°3 
largoDePlanilla:: Planilla -> Int 
largoDePlanilla    (Fin)       = 0 
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p 


esta:: Nombre -> Planilla -> Bool 
esta n      (Fin)       = False 
esta n (Registro n2 p)  = n == n2 || esta n p  

juntarPlanillas:: Planilla -> Planilla -> Planilla 
juntarPlanillas         Fin      p2 = p2 
juntarPlanillas (Registro n r2)  p2 = Registro n  (juntarPlanillas r2 p2) 

nivelesJerarquicos:: Equipo -> Int 
nivelesJerarquicos       (Becario n)          = 0
nivelesJerarquicos (Investigador n e1 e2 e3 ) = 1 +     
                                                nivelesJerarquicos e1 
                                                `max` 
                                                nivelesJerarquicos e2 
                                                `max` 
                                                nivelesJerarquicos e3 

b1 = Becario "Tincho"
b2 = Becario "Tincho2"
b3 = Becario "Fran"
e1 = Investigador "e1" (Investigador "e2" b1 b2 (Investigador "e3" b2 b1 (Becario "Romina"))) b2 b3


cantidadDeIntegrantes:: Equipo -> Int 
cantidadDeIntegrantes (Becario n) = 1 
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 
    1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3 
    

planillaDeIntegrantes:: Equipo -> Planilla 
planillaDeIntegrantes (Becario n) = Registro n Fin 
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n
        (planillaDeIntegrantes e1
            `juntarPlanillas` 
            planillaDeIntegrantes e2
            `juntarPlanillas` 
            planillaDeIntegrantes e3) 


----------------------------------------------------------------------------------------------
--                                  SECCIÓN 3

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) 
                | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) 


--CASO BASE (Dungeon)
-- Si x pertence al conjunto a entonces Habitacion x pertenece al 
-- conjunto dungeon a.

--CASO INDUCTIVO 1 (Dungeon)
-- Si m pertenece al conjunto Maybe A,
--    y d pertenece al conjunto Dungeon A,
--    entonces Pasaje m d pertenece al conjunto Dungeon A

--CASO INDUCTIVO 2 (Dungeon)
-- Si m' pertenece  al conjunto Maybe A,
--  d' pertenece al conjunto Dungeon A y 
--  n' pertenece al conjunto Dungeon A entonces 
--  Bifurcacion m' d' n' pertenece al conjunto Dungeon A 

-- Estructura 
-- f (Habitacion a)        = .... 
-- f (Pasaje m d )         = ... m ... f(d) 
-- f (Bifurcacion m di dd )= ... m ... f(di) ..f(dd)


cantidadDeBifurcaciones:: Dungeon a -> Int 
cantidadDeBifurcaciones (Habitacion a)        = 0
cantidadDeBifurcaciones (Pasaje m d )         = cantidadDeBifurcaciones(d) 
cantidadDeBifurcaciones (Bifurcacion m di dd) = 1 + cantidadDeBifurcaciones(di) + cantidadDeBifurcaciones(dd)


cantidadDePuntosInteresantes::  Dungeon a -> Int 
cantidadDePuntosInteresantes (Habitacion a)        = 0
cantidadDePuntosInteresantes (Pasaje m d )         = unoSiJust m + cantidadDePuntosInteresantes(d) 
cantidadDePuntosInteresantes (Bifurcacion m di dd )= unoSiJust m + cantidadDePuntosInteresantes(di) + cantidadDePuntosInteresantes(dd)


unoSiJust:: Maybe a -> Int 
unoSiJust (Just a) =  1 
unoSiJust Nothing  =  0


cantidadDePuntosVacios::  Dungeon a -> Int 
cantidadDePuntosVacios (Habitacion a)        = 0
cantidadDePuntosVacios (Pasaje m d )         = unoSiNothing m + cantidadDePuntosVacios(d) 
cantidadDePuntosVacios (Bifurcacion m di dd )= unoSiNothing m + cantidadDePuntosVacios(di) + cantidadDePuntosVacios(dd)

unoSiNothing:: Maybe a -> Int 
unoSiNothing Nothing  =  1
unoSiNothing (Just a) =  0 

cantPuntosInterCon :: Eq a => a -> Dungeon a -> Int
cantPuntosInterCon e (Habitacion x) = 1
cantPuntosInterCon e (Pasaje mx d) = unoSiTiene e mx + cantPuntosInterCon e d
cantPuntosInterCon e (Bifurcacion mx di dd) =  unoSiTiene e mx + cantPuntosInterCon e di + cantPuntosInterCon e dd

unoSiTiene:: Eq a => a -> Maybe a -> Int 
unoSiTiene e Nothing  = 0
unoSiTiene e (Just x) =	if e == x then 1 else 0 


esLineal:: Dungeon a -> Bool 
esLineal (Habitacion a) = True
esLineal (Pasaje m d )  = esLineal(d)
esLineal (Bifurcacion m di dd) = False 

-- data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) 
--                 | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) 

llenoDe::Eq a => a -> Dungeon a -> Bool 
llenoDe   e    (Habitacion a)        = e == a 
llenoDe   e    (Pasaje m d )         = esIgual e m &&  llenoDe e d 
llenoDe   e    (Bifurcacion m di dd) =  esIgual e m &&  (llenoDe e di) &&  (llenoDe e dd) 

esIgual::Eq a=> a -> Maybe a -> Bool 
esIgual e (Just a) = e == a 
esIgual e Nothing = False 