--                                          Sección II
--Ejercicio 1) Dada la siguiente definición
-- cuya intención es describir representaciones unarias de números naturales,
-- a. 
-- implementar las siguientes funciones por recursión estructural (o primitiva de
-- ser necesario):
-- i. 
evalN :: N -> Int
-- que describe el número representado por el
-- elemento dado.
evalN (Z)   = 0 
evalN (S x) = 1 + evalN x 

-- ii. 
addN :: N -> N -> N 
-- que describe la representación unaria de la
-- suma de los números representados por los argumentos. La
-- resolución debe ser exclusivamente simbólica, o sea, SIN calcular
-- cuáles son esos números.
addN  (Z)  r = r 
addN (S x) r = S (addN x r ) 

-- iii. 
prodN :: N -> N -> N 
-- que describe la representación unaria del
-- producto de los números representados por los argumentos. La
-- resolución debe ser exclusivamente simbólica.
prodN  (Z)  m = Z
prodN (S x) m = addN (prodN x m) m  

-- iv. 
int2N :: Int -> N 
-- que describe la representación unaria del
-- número dado usando el tipo N
int2N  0 = Z 
int2N  n = S  (int2N (n-1)) 


-- Ejercicio 2) Dada la siguiente definición
type NU = [()]
-- cuya intención es describir representaciones unarias de números como listas de
-- símbolos. El tipo () se lee Unit, y su único elemento es (); es equivalente a la
-- siguiente definición:
data Unit = Unit
-- a. 
-- implementar las siguientes funciones por recursión estructural (o primitiva de
-- ser necesario):

-- i. 
evalNU :: NU -> Int
-- , que describe el número representado por
-- el elemento dado.
evalNU [] = 0
evalNU (():xs) = 1 + evalNU xs

-- ii. 
succNU :: NU -> NU
-- , que describe la representación unaria del
-- resultado de sumarle uno al número representado por el argumento.
-- La resolución debe ser exclusivamente simbólica.
succNU xs = (): xs 

-- iii. 
addNU :: NU -> NU -> NU
-- , que describe la representación unaria
-- de la suma de los números representados por los argumentos. La
-- resolución debe ser exclusivamente simbólica.
addNU     [] ys = ys
addNU (x:xs) ys = x: addNU xs  ys

-- iv. 
nu2n :: NU -> N
-- , que describe la representación unaria dada por
-- el tipo N correspondiente al número representado por el argumento.
nu2n []     = Z 
nu2n (():xs)= S (nu2n xs)   

-- v. 
n2nu :: N -> NU
-- que describe la representación unaria dada por
-- el tipo NU correspondiente al número representado por el argumento
n2nu (Z)    = []
n2nu (S x)  = (): n2nu x 


-- Ejercicio 3) Dada la siguiente definición
-- cuya intención es describir representaciones binarias de números con el dígito
-- menos significativo a la izquierda, y siendo DigBin el tipo definido en el ejercicio 2
-- de la práctica 5. Es recomendable reutilizar las funciones definidas en el ejercicio
-- mencionado.
-- a. implementar las siguientes funciones por recursión estructural (o primitiva de
-- ser necesario):

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

-- i. 
evalNB :: NBin -> Int
-- -- que describe el número representado
-- -- por el elemento dado.
evalNB [] = 0
evalNB (x:xs) = 
  case x of
    I -> 1 + 2 * evalNB xs -- Si el dígito es 'I', sumamos 2 elevado al número de dígitos restantes y luego sumamos 1
    O -> 2 * evalNB xs -- Si el dígito es 'O', simplemente multiplicamos 2 por el valor calculado recursivamente

dbAsInt :: DigBin -> Int
--, que dado un símbolo que representa un
--dígito binario lo transforma en su significado como número.
dbAsInt I = 0
dbAsInt O = 1 

--ejemplos : 
n1 = [I,I,I,I] --15
n2 = [I,O,I,I] --13
n3 = [I,I,O,I] --11
n4 = [I,I,O,O] --No está normalizado, ya que no puede terminar en OO 
               --Deberia cambiarlo por  [I,I] 

-- ii. 
normalizarNB :: NBin -> NBin
normalizarNB [] = []
normalizarNB (b:bs) = normD b (normalizarNB bs)

normD :: DigBin -> NBin -> NBin
normD O [] = []
normD b bs = b : bs

type NBin = [DigBin] 
data DigBin = O | I deriving Show

-- iii. 
succNB :: NBin -> NBin
-- , que describe la representación binaria
-- normalizadadel resultado de sumarle uno al número representado
-- por el argumento. La resolución debe ser exclusivamente simbólica, y
-- no debe utilizar normalizarNB. 
-- PRECONDICION Se puede suponer  que el argumento está normalizado.
succNB    [] = []
succNB (x:xs)= sumarUno x  (succNB xs) 


sumarUno:: DigBin -> NBin -> NBin 
sumarUno I    []  = [I]
sumarUno I (b:bs)= if mismoDigito O b
                      then I: bs 
                      else b : sumarUno I bs 

mismoDigito :: DigBin -> DigBin -> Bool
mismoDigito O O = True 
mismoDigito I I = True 
mismoDigito _ _ = False

-- iv. 
addNB :: NBin -> NBin -> NBin
addNB xs ys = addNBConCarry xs ys O

addNBConCarry :: NBin -> NBin -> DigBin -> NBin
addNBConCarry [] [] O = [] -- Si ambos operandos están vacíos y no hay acarreo, el resultado es una lista vacía
addNBConCarry [] [] I = [I] -- Si ambos operandos están vacíos pero hay acarreo, el resultado es un acarreo
addNBConCarry xs [] carry = addNBConCarry xs [O] carry  -- Si una de las listas está vacía, el resultado es la otra lista
addNBConCarry [] ys carry = addNBConCarry [O] ys carry
-- Caso recursivo: Sumar los dígitos actuales y manejar cualquier acarreo
addNBConCarry (x:xs) (y:ys) carry =  sumarDigitosConCarry x y carry : addNBConCarry xs ys nextCarry
    where
        -- Determinar el siguiente acarreo-
        nextCarry = snd (addDBConCarry x y carry)

-- Función auxiliar que suma dos dígitos con un posible acarreo y devuelve el dígito resultante y el acarreo
sumarDigitosConCarry :: DigBin -> DigBin -> DigBin -> DigBin
sumarDigitosConCarry x y carry = fst (addDBConCarry x y carry)

-- Función auxiliar que suma dos dígitos con un posible acarreo y devuelve el dígito resultante y el acarreo
addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDBConCarry O O O = (O, O)
addDBConCarry O O I = (I, O)
addDBConCarry O I O = (I, O)
addDBConCarry O I I = (O, I)
addDBConCarry I O O = (I, O)
addDBConCarry I O I = (O, I)
addDBConCarry I I O = (O, I)
addDBConCarry I I I = (I, I)


data N = Z | S N deriving Show
--v. 
nb2n :: NBin -> N
-- , que describe la representación unaria dada
-- por el tipo N correspondiente al número representado por el
-- argumento.
nb2n    []  = Z
nb2n (x:xs) =  case x of
                I -> addN (S(Z))  (producto) -- Si el dígito es 'I', hacemos lo mismo que con los enteros pero usando las funciones de N 
                O ->  producto    -- Si el dígito es 'O', lo mismo
                where 
                  producto = prodN (S(S Z))  (nb2n xs) -- 2 * eval xs 

-- vi. 
n2nb :: N -> NBin
-- , que describe la representación binaria
-- normalizada dada por el tipo NBin correspondiente al número
-- representado por el argumento
n2nb     Z = [O] 
n2nb (S x) =  I : normalizarNB (n2nb x)  


--                                      Sección III 


data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA


-- a. implementar las siguientes funciones por recursión estructural (o primitiva de
-- ser necesario):
-- i. 
evalExpA :: ExpA -> Int
--, que describe el número que resulta
-- de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) =  evalExpA e1 * evalExpA e2


-- ii. 
simplificarExpA :: ExpA -> ExpA
--, que describe una
-- expresión aritmética con el mismo significado que la dada, pero que
-- no tiene sumas del número 0, ni multiplicaciones por 1 o por 0. La
-- resolución debe ser exclusivamente simbólica.
simplificarExpA (Cte n) = Cte n
simplificarExpA (Suma e1 e2) = 	simpSuma (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) =	simpProd (simplificarExpA e1) (simplificarExpA e2)

simpSuma :: ExpA -> ExpA -> ExpA
simpSuma e1 (Cte 0) = e1
simpSuma (Cte 0) e2 = e2
simpSuma e1 e2 = Suma e1 e2

simpProd :: ExpA -> ExpA -> ExpA
simpProd e1 (Cte 1) = e1
simpProd (Cte 1) e2 = e2
simpProd e1 (Cte 0) = Cte 0
simpProd (Cte 0) e2 = Cte 0
simpProd e1 e2 = Prod e1 e2

-- iii. 
cantidadDeSumaCero :: ExpA -> Int
--, que describe la cantidad
-- de veces que aparece suma cero en la expresión aritmética dada. La
-- resolución debe ser exclusivamente simbólica.
cantidadDeSumaCero (Cte n) = 0
cantidadDeSumaCero (Suma e1 e2) = 	unoSiSumaCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) =	cantidadDeSumaCero e1 + cantidadDeSumaCero e2

cantidadDeSumaCero :: ExpA -> ExpA -> Int
unoSiSumaCero (Cte 0) e2 = 1
unoSiSumaCero e1 (Cte 0) = 1
unoSiSumaCero e1 e2 = 0
