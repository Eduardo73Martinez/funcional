--dada
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
--a. implementar las siguientes funciones por recursión estructural (o primitiva en
--caso de ser necesario):

expA2ea :: ExpA -> EA
-- que describe una expresión aritmética
-- representada con el tipo EA, cuya estructura y significado son los
-- mismos que la dada.
expA2ea   (Cte n )   = Const n
expA2ea  (Suma e1 e2)= BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2 )= BOp Mul (expA2ea e1) (expA2ea e2) 

ea2ExpA :: EA -> ExpA
--, que describe una expresión aritmética
-- representada con el tipo ExpA, cuya estructura y significado son los
-- mismos que la dada.
ea2ExpA      (Const n)=  Cte n 
ea2ExpA (BOp op ei ed)=  operation op (ea2ExpA ei) (ea2ExpA ed) 

operation:: BinOp -> ExpA -> ExpA -> ExpA
operation Sum e1 e2 = Suma e1 e2 
operation Mul e1 e2 = Prod e1 e2 

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) =  evalExpA e1 * evalExpA e2

evalEA :: EA -> Int
--, que describe el número que resulta de
-- evaluar la cuenta representada por la expresión aritmética dada.
evalEA      (Const n)=  n  
evalEA (BOp op ei ed)= case op of 
                        sum ->  (evalEA ei) + (evalEA ed)  
                        Mul ->  (evalEA ei) * (evalEA ed)  

-- b. demostrar la siguiente propiedad:
-- i. ea2ExpA . expA2ea = id
--     -> ap.p.extencionabilidad
--         para todo x. 
--     ¿ ea2ExpA . expA2ea x = id x ? 
--      -> por def (.) es equivalente a: 

--     para todo x. 
--     ¿ ea2ExpA (expA2ea x) = id x ? 

-- caso base ) 
--     x = Cte n 

-- lado izq) 
--     ea2ExpA (expA2ea (Cte n))
--     = def. expA2e, x<- Cte n
--     ea2ExpA (Const n)
--     = def. ea2ExpA, x<- Const n 
--     Cte n

-- lado der) 
--     id (Cte n)
--     = ap. def id 
--     (Cte n) 
--     DEMOSTRADO EL CASO BASE! 

-- caso inductivo 1 ) 
--     x = (Suma e1 e2)
--     HI.1 ea2ExpA (expA2ea e1)= id e1
--     HI.2 ea2ExpA (expA2ea e2) = id e2  
--     TI. ¿ea2ExpA (expA2ea (Suma e1 e2)) = id (Suma e1 e2) ? 

-- lado izq) 
--     ea2ExpA (expA2ea (Suma e1 e2))
--     = def. expA2e, x<-Suma e1 e2
--     ea2ExpA ( BOp Sum (expA2ea e1) (expA2ea e2))
--     = def. ea2ExpA, x<- BOp Sum (expA2ea e1) (expA2ea e2)
--     Suma (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2) )
--     = HI1 y HI2 
--     Suma (id e1) (id e2)
    
-- lado der) 
--     id (Suma e1 e2)
--     =por def. id 
--     Suma (id e1) (id e2)
    
--     DEMOSTRADO!

-- Caso inductivo 2 )
--     x = (Prod e1 e2)
--     HI.1 ea2ExpA (expA2ea e1)= id e1
--     HI.2 ea2ExpA (expA2ea e2) = id e2  
--     TI. ¿ea2ExpA (expA2ea (Prod e1 e2)) = id (Prod e1 e2) ? 

-- lado izq) 
--     ea2ExpA (expA2ea (Prod e1 e2)) 
--     = def. expA2ea, x<- Prod e1 e2 
--     ea2ExpA ( Mul (expA2ea e1) (expA2ea e2)  )
--     = def ea2ExpA , x <-  Mul (expA2ea e1) (expA2ea e2)
--     Prod (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
--     = HI1 e HI2 
--     Prod (id e1) (id e2)

-- lado der) 
--     id (Prod e1 e2)
--     =por def. id 
--     Prod (id e1) (id e2)
--     DEMOSTRADO!
-----------------------------------------------------------------------------

-- ii. expA2ea . ea2ExpA = id
-- -> ap.p.extencionabilidad
--         para todo x. 
--      ¿ expA2ea . ea2ExpA x = id x ? 
--       -> por def (.) es equivalente a: 

--      para todo x. 
--      ¿ expA2ea (ea2ExpA x)= id x? 

-- caso base ) 
--     x = Const n

-- lado izq) 
--     expA2ea (ea2ExpA (Const n)) 
--     = def ea2ExpA x, 
--     expA2ea (Cte n ) 
--     = def expA2ea x 
--     Const n
    
-- lado der) 
--     id Const n 
--     = p. def id 
--     Const n 

-- caso inductivo ) 
--     x = BOp op ei ed
--     HI.1 expA2ea (ea2ExpA ei) = id ei 
--     HI.2 expA2ea (ea2ExpA ed) = id ed 
--     TI ¿expA2ea (ea2ExpA (BOp op ei ed))= id (BOp op ei ed) ? 

-- lado izq) 
--     expA2ea (ea2ExpA (BOp op ei ed)) 
--     = def ea2ExpA, x<- BOp op ei ed 
--     expA2ea ( operation op  (ea2ExpA ei) (ea2ExpA ed) )
--     = def expA2ea , x<- operation op expA2ea (ea2ExpA ei) (ea2ExpA ed)
--     Bop operation op (expA2ea (ea2ExpA ei)) (expA2ea (ea2ExpA ed))
--     = HI1 e HI2 
--     Bop operation op (id ei ) (id ed )
--     = por def operacion 
--     Bop op (id ei ) (id ed )

-- lado der) 
--     id (BOp op ei ed)
--     = def id 
--     BOp op (id ei) (id ed)
--     DEMOSTRADO! 
--------------------------------------------------------------------------------------

-- iii. evalExpA . ea2ExpA = evalEA
--     -> ap.p.extencionabilidad
--         para todo x. 
--      ¿ evalExpA . ea2ExpA x= evalEA x ? 
--       -> por def (.) es equivalente a: 

--      para todo x. 
--      ¿ evalExpA (ea2ExpA x)= evalEA x ? 

-- caso base) 
--     x= (Const n)

-- lado izq) 
--     evalExpA (ea2ExpA (Const n))
--     =def ea2ExpA , x<- (Const n)
--     evalExpA  (Cte n ) 
--     = def evalExpA 
--     n 

-- lado der) 
--     evalEA (Const n) 
--     = def evalExpA  
--     n 

-- caso Inductivo) 
--     x= (BOp op ei ed)
--     HI. evalExpA . ea2ExpA (ei) = evalEA  ei
--     HI. evalExpA . ea2ExpA (ed) = evalEA  ed
--     TI. ¿ evalExpA . ea2ExpA (BOp op ei ed) = evalEA  (BOp op ei ed) ? 

-- lado izq) 
--     evalExpA (ea2ExpA (BOp op ei ed)))
--     =def ea2ExpA , x<- (BOp op ei ed)
--     evalExpA  (operation op (ea2ExpA ei) (ea2ExpA ed)) 
--     = def evalExpA, x<- (operation op (ea2ExpA ei) (ea2ExpA ed)) 

--      2 casos------> (operation op)  =Suma ||  (operation op)  =Prod 
--     caso Suma) 
--             evalExpA  (Suma (ea2ExpA ei) (ea2ExpA ed)) 
--             = def. evalExpA , x<- Suma (ea2ExpA ei) (ea2ExpA ed)
--             evalExpA (ea2ExpA ei) + evalExpA (ea2ExpA ed)
--             = HI1 
--             evalEA  ei + evalEA  ed

--     caso Prod)
--             evalExpA  (Prod (ea2ExpA ei) (ea2ExpA ed)) 
--             = def. evalExpA , x<- Prod (ea2ExpA ei) (ea2ExpA ed)
--             evalExpA (ea2ExpA ei) * evalExpA (ea2ExpA ed)
--             = HI2 
--             evalEA  ei * evalEA  ed
-- lado der)
--     caso Suma) 
--             evalEA  (BOp op ei ed)
--             = def evaA, x<-BOp op ei ed 
--             evalEA ei + evalEA ed  

--     caso Prod) 
--             evalEA  (BOp op ei ed)
--             = def evaA, x<-BOp op ei ed 
--             evalEA ei * evalEA ed  

-- DEMOSTRADO para todos los casos ! 
--------------------------------------------------------------------------------------

-- -- iv. evalEA . expA2ea = evalExpA
--     -> ap.p.extencionabilidad
--         para todo x. 
--      ¿ evalEA . expA2ea x = evalExpA x? 
--       -> por def (.) es equivalente a: 

--      para todo x. 
--      ¿ evalEA  (expA2ea x )= evalExpA x ? 

-- caso base) 
--     x= (Cte n )

-- lado izq) 
--     evalEA  (expA2ea (Cte n ))
--     = def expA2ea , x <- (Cte n )
--     evalEA  (Const n)
--     = def evalEA
--     n 

-- lado der) 
--     evalExpA (Cte n )
--     = def evalExpA 
--     n 

-- caso inductivo 1 ) 
--     x = (Suma e1 e2)
--     HI1. evalEA  (expA2ea e1 )= evalExpA e1
--     HI2. evalEA  (expA2ea e2 )= evalExpA e2
--     TI. ¿ evalEA  (expA2ea (Suma e1 e2) )= evalExpA (Suma e1 e2)?

-- lado izq) 
--     evalEA  (expA2ea (Suma e1 e2) )
--     = def expA2ea , x<- (Suma e1 e2) 
--     evalEA (BOp Sum (expA2ea e1) (expA2ea e2))
--     = def evalEA 
--     evalEA (evalEA ei) + evalEA (evalEA ed) 
--     = HI1 y HI2
--     evalExpA e1 + evalExpA e2 

-- lado der)  
--     evalExpA (Suma e1 e2)
--     = def evalExpA 
--     evalExpA e1 + evalExpA e2 

--     DEMOSTRADO! 

-- caso inductivo 2) 
--     x = (Prod e1 e2)
--     HI1. evalEA  (expA2ea e1 )= evalExpA e1
--     HI2. evalEA  (expA2ea e2 )= evalExpA e2
--     TI. ¿ evalEA  (expA2ea (Prod e1 e2) )= evalExpA (Prod e1 e2)?

-- lado izq) 
--     evalEA  (expA2ea (Prod e1 e2) )
--     = def expA2ea , x<- (Prod e1 e2) 
--     evalEA (BOp Prod (expA2ea e1) (expA2ea e2))
--     = def evalEA 
--     evalEA (evalEA ei) * evalEA (evalEA ed) 
--     = HI1 y HI2
--     evalExpA e1 * evalExpA e2 

-- lado der)  
--     evalExpA (Prod e1 e2)
--     = def evalExpA 
--     evalExpA e1 * evalExpA e2 
--     DEMOSTRADO! 
--------------------------------------------------------------------------------------

--Ejercicio 2) Dada la siguiente definición

--que representa una estructura de árboles binarios;
--a. implementar las siguientes funciones por recursión estructural (o primitiva en
--caso de ser necesario):

--i. 
cantidadDeHojas :: Arbol a b -> Int
--, que describe la
--cantidad de hojas en el árbol dado.
cantidadDeHojas          (Hoja _)  = 1 
cantidadDeHojas (Nodo _ (ti) (td)) = cantidadDeHojas ti + cantidadDeHojas td 

--ii. 
cantidadDeNodos :: Arbol a b -> Int
--, que describe la
--cantidad de nodos en el árbol dado.
cantidadDeNodos          (Hoja _)  = 0
cantidadDeNodos (Nodo _ (ti) (td)) = 1+  cantidadDeNodos ti + cantidadDeNodos td 

--iii. 
cantidadDeConstructores :: Arbol a b -> Int
--, que
--describe la cantidad de constructores en el árbol dado.
cantidadDeConstructores          (Hoja _)  = 1 
cantidadDeConstructores (Nodo _ (ti) (td)) = 1+ cantidadDeConstructores ti + cantidadDeConstructores td 


data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

data BinOp = Sum | Mul

data EA = Const Int | BOp BinOp EA EA

--iv. 
ea2Arbol :: EA -> Arbol BinOp Int
--, que describe la
--representación como elemento del tipo Arbol BinOp Int de la
--expresión aritmética dada.
ea2Arbol       (Const x)   = Hoja x
ea2Arbol (BOp op ei ed) = Nodo op (ea2Arbol ei)  (ea2Arbol ed )

--b. demostrar la siguiente propiedad:
--para todo t :: Arbol a b 
-- cantidadDeHojas t + cantidadDeNodos t = cantidadDeConstructores t

-- caso base ) 
--     t= (Hoja _)  

-- lado izq) 
--     cantidadDeHojas (Hoja _)  + cantidadDeNodos (Hoja _) 
--     = def cantidadDeHojas, x<- (Hoja _)  y de def cantidadDecantidadDeNodos , x<- (Hoja _) 
--     1 + 0
--     = por art. 
--     2 
-- lado der) 
--     cantidadDeConstructores (Hoja _)
--     = por def. cantidadDeConstructores, x<-(Hoja _)
--     1 

-- caso inductivo ) 
--     t = (Nodo x (ti) (td)) 
--     HI1. cantidadDeHojas (ti) + cantidadDeNodos (ti) = cantidadDeConstructores (ti)
--     HI2. cantidadDeHojas (td) + cantidadDeNodos (td) = cantidadDeConstructores (td)
--     TI. ¿cantidadDeHojas (Nodo x (ti) (td)) + cantidadDeNodos (Nodo x (ti) (td))  =
--              cantidadDeConstructores (Nodo x (ti) (td)) ? 


-- lado izq) 
--     cantidadDeHojas (Nodo x (ti) (td)) + cantidadDeNodos (Nodo x (ti) (td)) 
--     =  def cantidadDeHojas, x<-...  y de def cantidadDecantidadDeNodos , x<- ...
--     cantidadDeHojas ti + cantidadDeHojas td + 1 +  cantidadDeNodos ti + cantidadDeNodos td  
--     = ORDENAMOS DE MODO QUE QUEDN CANTDEHOJAS Y CANTIDADDENODOS DE MANERA CONSECUTIVAS
--     cantidadDeHojas ti + cantidadDeNodos ti + cantidadDeHojas td  +  cantidadDeNodos td + 1
--     = HI1 E HI2  
--     cantidadDeConstructores (ti) +  cantidadDeConstructores (td)  + 1 

-- lado der) 
--     cantidadDeConstructores (Nodo x (ti) (td)) 
--     = POR def cantidadDeConstructores, x<- Nodo x (ti) (td) 
--     1 + cantidadDeConstructores ti + cantidadDeConstructores td  
--     = acomodamos por arit 
--     cantidadDeConstructores (ti) +  cantidadDeConstructores (td) + 1 
--     DEMOSTRADO ! 
-----------------------------------------------------------------------------------------------------------
--Ejercicio 3) Dada la siguiente definición
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)


anyT :: (a -> Bool) -> Tree a -> Bool
-- , que indica si en el
-- árbol dado hay al menos un elemento que cumple con el predicado
-- dado.
anyT f         (EmptyT) = False 
anyT f (NodeT v ti td ) = f (v) || anyT f ti || anyT f td 

countT :: (a -> Bool) -> Tree a -> Int
-- , que describe la
-- cantidad de elementos en el árbol dado que cumplen con el predicado
-- dado
countT f         (EmptyT) = 0 
countT f (NodeT v ti td ) = unoSiCeroSiNo (f (v)) + countT f ti + countT f td 

unoSiCeroSiNo:: Bool -> Int
unoSiCeroSiNo True = 1 
unoSiCeroSiNo False= 0

heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol1 La altura para EmptyT es 0, y para una hoja es 1.
heightT EmptyT           = 0 
heightT (NodeT x ai ad)  = 1 + max ( heightT ai)  ( heightT ad) 

-- b. demostrar las siguientes propiedades:

-- i. heightT = length . ramaMasLarga
    ----- DEMOSTRADO EN CLASE, VER ARCHIVO tree.hs 

mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT  EmptyT          = EmptyT 
mirrorT (NodeT x ai ad)  = NodeT x ( mirrorT ad) (mirrorT ai)

listInOrder :: Tree a -> [a]
-- , que describe la lista in order con los
-- elementos del árbol dado
listInOrder EmptyT           = []
listInOrder (NodeT x ai ad)  = listInOrder ai  ++ [ x ] ++ listInOrder ad 


-- A:: [a] -> [a] 
-- reverse    [] = []
-- reverse (x:xs)= reverse xs ++ [x]

-- ii. reverse . listInOrder = listInOrder . mirrorT 
--     =  APLICAMOS P.extencionabilidad 
--      reverse . listInOrder x = listInOrder . mirrorT x
--     = POR DEF (.) 
--     reverse (listInOrder x )= listInOrder (mirrorT x)

--     PARA TODO X, ¿ reverse (listInOrder x )= listInOrder (mirrorT x) ? 

--     DEMOSTRAREMOS POR INDUCCION ESTRUCTUAL 

-- CASO BASE ) 
--     x =  EmptyT 

--     reverse (listInOrder EmptyT )= listInOrder (mirrorT EmptyT)
--     trivial! 

-- CASO INDUCTIVO) 
--     x= (NodeT x' ai ad) 
--     HI1. reverse (listInOrder ai)= listInOrder (mirrorT ai) 
--     HI2. reverse (listInOrder ad)= listInOrder (mirrorT ad) 
--     TI. ¿ reverse (listInOrder (NodeT x' ai ad)  )= listInOrder (mirrorT (NodeT x' ai ad) )  

-- lado izq) 
--     reverse (listInOrder (NodeT x' ai ad)  )
--     = def. listInOrder, x<- NodeT x' ai ad 
--     reverse (listInOrder ai  ++ [ x'] ++ listInOrder ad )
--     = por propiedad de reverse.
--     reverse (listInOrder ad ) ++ reverse [ x'] ++ reverse (listInOrder ai)
--     = HI1. HI2. 
--     listInOrder (mirrorT ad) ++ [x'] ++ listInOrder (mirrorT ai)  

-- lado der) 
--     listInOrder (mirrorT (NodeT x' ai ad) )
--     = def mirrorT, x<- NodeT x' ai ad 
--     listInOrder( NodeT x' ( mirrorT ad) (mirrorT ai))
--     = def. listInOrder, x<- NodeT x' ( mirrorT ad) (mirrorT ai) 

--     listInOrder (mirrorT ad) ++ [x'] listInOrder (mirrorT ai)  

--     DEMOSTRADO ! 


--Ejercicio 4) Dada la siguiente definición
-- cuya intención es describir una representación no lineal de listas no vacías;
data AppList a = Single a | Append (AppList a) (AppList a)

-- a. implementar las siguientes funciones por recursión estructural (o primitiva en
-- caso de ser necesario):

-- i. 
lenAL :: AppList a -> Int
--, que describe la cantidad de
-- elementos de la lista.
lenAL     (Single x) = 1 
lenAL (Append li ld) =  lenAL li + lenAL ld 

-- ii. 
consAL :: a -> AppList a -> AppList a
--, que describe la
-- lista resultante de agregar el elemento dado al principio de la lista
-- dada.
consAL x ys = Append (Single x) ys

-- iii. 
headAL :: AppList a -> a
--, que describe el primer elemento de
-- la lista dada.
headAL     (Single x) = x 
headAL (Append li _) = headAL li 

-- iv. 
tailAL :: AppList a -> AppList a
--, que describe la lista
-- resultante de quitar el primer elemento de la lista dada.
tailAL (Single _)      = error "tailAL: lista vacía"  -- No hay cola para una lista con un solo elemento
tailAL (Append (Single _) ys) = ys  -- Si la primera sublista es un solo elemento, devuelve la segunda sublista
tailAL (Append xs ys) = Append (tailAL xs) ys  -- Si es una concatenación, quita el primer elemento de la primera sublista

-- v. 
snocAL :: a -> AppList a -> AppList a
--, que describe la
-- lista resultante de agregar el elemento dado al final de la lista dada.
snocAL zs (Single ys)    = Append  (Single ys) (Single zs) 
snocAL zs (Append ys xs) = Append ys  (snocAL zs  xs ) 

-- vi. 
lastAL :: AppList a -> a
--, que describe el último elemento de
-- la lista dada.
lastAL (Single ys)    = ys 
lastAL (Append ys xs) = lastAL xs 

-- vii. 
initAL :: AppList a -> AppList a
--, que describe la lista dada
-- sin su último elemento.
initAL            (Single x)  = error "initAL: Lista vacia"
initAL (Append ys (Single _)) = ys 
initAL         (Append ys xs) = Append ys (initAL xs) 

-- viii. 
reverseAL :: AppList a -> AppList a
--, que describe la lista
-- dada con sus elementos en orden inverso.
reverseAL   (Single x)  = (Single x)
reverseAL (Append ys xs)= Append  (reverseAL xs) (reverseAL ys)

-- ix. 
elemAL :: Eq a => a -> AppList a -> Bool
--, que indica si el
-- elemento dado se encuentra en la lista dada.
elemAL zs (Single ys)    = zs == ys
elemAL zs (Append ys xs) = elemAL zs ys || elemAL zs xs 

-- x. 
appendAL :: AppList a -> AppList a -> AppList a
--, que
-- describe el resultado de agregar los elementos de la primera lista
-- adelante de los elementos de la segunda.
-- NOTA: buscar la manera más eficiente de hacerlo.
        -- Para implementar appendAL de manera eficiente, 
        -- podemos explorar una estrategia que evite recorrer 
        -- toda la primera lista en cada llamada recursiva.
        -- Una manera eficiente de hacerlo es usar una estructura 
        -- de datos adicional para representar la lista inversa 
        -- temporalmente mientras construimos la concatenación. 
        -- Luego, podemos invertir la lista resultante en un solo paso al final. 
appendAL xs ys = reverseAL (appendAL' (reverseAL xs) ys)

-- Función auxiliar para concatenar listas invertidas
appendAL' :: AppList a -> AppList a -> AppList a
appendAL' (Single x) ys     = consAL x ys
appendAL' (Append xs zs) ys = Append xs (appendAL' zs ys)


-- xi. 
appListToList :: AppList a -> [a]
--, que describe la
-- representación lineal de la lista dada
appListToList (Single x)      = [x]
appListToList (Append xs zs)  = appListToList xs ++ appListToList zs 

-- i.
-- para todo xs :: AppList a. para todo ys :: AppList a.
-- lenAL (appendAL xs ys) = lenAL xs + lenAL ys 

-- caso base ) 
-- 	xs = (Single _) 
-- 	ys = (Single _) 


-- lado izq) 
-- 	lenAL (appendAL (Single _)  (Single _) ) 
-- 	= def appendAL, x <- (Single _)  (Single _)
-- 	lenAL (  reverseAL (appendAL' (reverseAL ((Single _)) (Single _)) ) 
-- 	= def reverseAL 
-- 	lenAL (  reverseAL (appendAL' (Single _) (Single _)) ) 
-- 	= def appendAL , x<- (Single _) (Single _)
-- 	lenAL (  reverseAL (consAL (Single _) (Single _)) ) 
-- 	= def constAl 1 
-- 	lenAL (reverseAL (Append (Single _) (Single _)))
-- 	= def reverseAL , x<- Append (Single _) (Single _)
-- 	lenAL (Append  (reverseAL (Single _)) (reverseAL (Single _)))
-- 	= def reverseAL , x<-(Single _)
-- 	lenAL (Append (Single _) (Single _))
-- 	= def lenAL , x<-  (Single _)
-- 	 1 + lenAL (Single _)
-- 	= def lenAL , x<-  (Single _)
-- 	 1 + 1 = 2 


-- lado der)  
-- 	lenAL (Single _) + lenAL (Single _) 
-- 	= def lenAL 
-- 	1 + 1 = 2 
-- 	DEMOSTRADO ! 
	
-- caso inductivo) 
-- 	xs =(Append xs' zs)
-- 	HI. lenAL (appendAL (Append xs' zs) ys) = lenAL (Append xs' zs) + lenAL ys 
-- 	TI.¿ lenAL (appendAL (Append xs' zs) ys) = lenAL (Append xs' zs) + lenAL ys ? 
	
-- lado izq) 
-- 	lenAL (appendAL (Append xs' zs) ys)
-- 	= def appendAL, x <- (Append xs' zs)   ys
-- 	lenAL (reverseAL (appendAL' (reverseAL (Append xs' zs)) ys))
-- 	= def reverseAL , x <- Append xs' zs 
-- 	lenAL (reverseAL (appendAL' (Append (reverseAL xs') (reverseAL zs)) ys))
-- 	= de. appendAL' ,x<- (Append (reverseAL xs') (reverseAL zs)) ys 
-- 	lenAL (reverseAL ( Append (reverseAL xs') appendAL' (reverseAL zs) ys))
-- 	= def reverseAL 2 
-- 	lenAL (  Append (reverseAL (reverseAL xs') ) reverseAL (appendAL' (reverseAL zs) ys)))
-- 	= def reverse 
-- 	lenAL (  Append (  xs')  reverseAL (appendAL' (reverseAL zs) ys)))
-- 	= def reverse 
-- 	lenAL ( Append xs' (appendAL' zs ys) )
-- 	= POR DEF. Append						 -- aplique p. que no se si son ciertas! 
-- 	lenAL ( appendAL (Append xs' zs) ys) 	 -- appendAL' LO CAMBIÉ POR appendAL
-- 	= hi 
-- 	lenAL (Append xs' zs) + lenAL ys 		-- CASI LLEGO

-- lado der) 
-- 	lenAL (Append xs' zs) + lenAL ys
-- 	DEMOSTRADO

-- -- ii. reverseAL . reverseAL = id
-- 	= APP. P extencionabilidad
-- 	reverseAL . reverseAL x = id x 
-- 	= POR def (.) 
-- 	reverseAL(reverseAL x ) = id x 

-- 	para todo x, ¿ reverseAL(reverseAL x ) = id x ?
-- 	voy a demostrarlo por INDUCCION 

-- caso base) 
-- 	x = (Single x) 
-- 	reverseAL(reverseAL (Single x) ) = id (Single x) 
-- 	trivial! 

-- caso inductivo)  
-- 	x = (Append ys xs)
-- 	HI1. reverseAL(reverseAL ys ) = id ys
-- 	HI2. reverseAL(reverseAL xs ) = id xs
-- 	Ti . reverseAL(reverseAL (Append ys xs) ) = id (Append ys xs)

-- lado izq)
-- 	reverseAL(reverseAL (Append ys xs) ) 
-- 	= def reverseAL, x<- (Append ys xs)
-- 	reverseAL ( Append  (reverseAL (ys) (reverseAL (xs)))
-- 	= def reverseAL, x<- Append  (reverseAL (Append ys xs)) (reverseAL (Append ys xs))
-- 	Append  reverseAL (reverseAL (ys)) reverseAL (reverseAL (xs)))
-- 	= HI 
-- 	Append  id (ys)  id ((xs))
-- 	= por def id 
-- 	id (Append  ys xs)

-- lado der) 
-- 	id (Append  ys xs)
-- 	DEMOSTRADO! 

--									falta este 
-- iii. (reverseAL .) . flip consAL . reverseAL = snocAL


--Ejercicio 5) Dadas las siguientes definiciones
data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
data Color = RGB Int Int Int
type Image = QuadTree Color

-- donde QuadTree representa a la estructura de los árboles cuaternarios, Color
-- representa a los colores con precisión TrueColor, e Image representa imágenes
-- cuadradas de tamaños arbitrarios;
-- a. implementar las siguientes funciones por recursión estructural (o primitiva en
-- caso de ser necesario):

heightQT :: QuadTree a -> Int
-- , que describe la altura del
-- árbol dado.
heightQT 			(LeafQ x) = 0
heightQT ( NodeQ t1 t2 t3 t4) = 1 +  (heightQT t1) `max` (heightQT t2) `max`  (heightQT t3) `max` (heightQT t4) 

countLeavesQT :: QuadTree a -> Int
-- , que describe la
-- cantidad de hojas del árbol dado.
countLeavesQT 			(LeafQ _) = 1 
countLeavesQT ( NodeQ t1 t2 t3 t4) = (heightQT t1) + (heightQT t2) +  (heightQT t3) + (heightQT t4) 

sizeQT :: QuadTree a -> Int
-- , que describe la cantidad de
-- constructores del árbol dado.
sizeQT 			(LeafQ _)  = 1 
sizeQT ( NodeQ t1 t2 t3 t4)=  1 + (heightQT t1) + (heightQT t2) +  (heightQT t3) + (heightQT t4) 

compress :: QuadTree a -> QuadTree a
-- , que describe el
-- árbol resultante de transformar en hoja todos aquellos nodos para los
-- que se cumpla que todos los elementos de sus subárboles son
-- iguales.
compress = undefined

uncompress :: QuadTree a -> QuadTree a
--, que describe el
-- árbol resultante de transformar en nodo (manteniendo el dato de la
-- hoja correspondiente) todas aquellas hojas que no se encuentren en
-- el nivel de la altura del árbol.
uncompress = undefined

render :: Image -> Int -> Image
-- , que describe la imagen
-- dada en el tamaño dado.
-- Precondición: el tamaño dado es potencia de 4 y es mayor o igual a la
-- altura del árbol dado elevado a la 4ta potencia.
-- NOTA: Una imagen tiene tamaño t cuando todas las hojas se
-- encuentran en el nivel log4(t).
-- AYUDA: Esta operación es similar a uncompress, pero pudiendo
-- variar la altura del árbol
render = undefined