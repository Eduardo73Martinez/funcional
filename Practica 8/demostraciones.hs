-- Ejercicio 2) Demostrar por inducción estructural las siguientes propiedades:
-- a. para todo xs. para todo ys.
-- length (xs ++ ys) = length xs + length ys

a.
 ppio de ext.
 length (xs ++ ys) = length xs + length ys

 caso base 1 ) 
    xs = [] 

¿ length ([] ++ ys) = length [] ++ length ys ?

lado izq 
    length ([] ++ ys)
    0 + length ys 
    length ys 
lado der 
    length [] ++ length ys
    0 + length ys 
    length ys 

    es igual, vale en el caso base! 

caso inductivo  
     xs = z:zs 
     HI) length (zs ++ ys) = length zs ++ length ys 
     TI) length (z:zs ++ ys) = length z:zs ++ length ys 


lado izq 
    length (z:zs ++ ys) 
    = def length x <- z:zs 
    1 + length (zs ++ ys) 
    = por HI 
    1 + length zs ++ length ys 

lado Der 
    length z:zs ++ length ys 
    = def length, x <- z:zs 
    1 + length zs ++ length ys 

    COMO EN AMBOS LADOS OBTENGO LO MISMO ENTONCES QUEDA DEMOSTRADO 

-- ------------------------------------------------------
-- b. count (const True) = length
ppio de ext.
¿ count (const True) xs = length xs ?

caso base) 
    xs = [] 

lado izq 
    count (const True) [] 
    = def count 
    0 

lado Der 
    length [] 
    = def length, x <- [] 
    0 
    Demostrado para el caso base 

caso inductivo) 
    xs = z:zs 
    HI. count (const True) zs = length zs  
    TI. ¿count (const True) z:zs = length z:zs ?

lado izq ) 
    count (const True) (z:zs) 
    = def count, x <- (const True) (z:zs) 
    1 + count (const True) (zs) 
    = HI. 
    1 + length zs   

lado Der) 
    length z:zs 
    = def length, x <- (z:zs) 
    1 + length zs 

    DEMOSTRADO ! 
-- ------------------------------------------------------
-- c. elem = any . (==)
ppio de ext.
¿ elem x xs = any ((== ) ) xs ?
para todo x perteneciente xs  

caso base) 
    xs = [] 

    ¿ elem x xs = any ((== ) ) xs ?
    trivial, en ambos casos es falso por lo cual está demostrado 

caso inductivo) 
    xs = z:zs 
    HI. elem x zs = any ((== )x ) zs
    TI. ¿ elem x (z:zs) = any ((== ) x) (z:zs) ?
    
lado izq ) 
    elem x (z:zs) 
    = def elem, x <- x' (z:zs)
    x == z || elem x zs 
    = por HI 
    x == z || any ((== )x ) zs 

lado Der) 
    any ((== ) x) (z:zs) 
    = def any, x<- ((==) x' )(z:zs) 
     x == z || any((==) x) xs

     DEMOSTRADO ! llegamos a lo mismo en ambos lados 
-- ------------------------------------------------------

-- d. para todo x. any (elem x) = elem x . concat
ppio de ext.
    para todo x.                         
    ¿ any (elem x) xs  = elem x ( concat xs ) ?

caso base ) 
    xs = []
lado izq ) 
    any (elem x) [] 
    = def  any, x<- (elem x) []  
    False 
lado der ) 
    elem x ( concat xs )
    = def elem x<- (elem x) [] 
    False 
    Demostrado ! 

Caso inductivo ) 
    xs = z:zs 
    HI. any (elem x) zs = elem x (concat zs)
    TI. any (elem x) (z:zs) = elem x (concat (z:zs)) 

lado izq ) 
    any (elem x) (z:zs) 
    = def any, x<-(elem x) (z:zs) 
    elem x z || any (elem x) zs 
    = HI 
    elem x z ||  elem x (concat zs)
    
lado der ) 
    elem x (concat (z:zs)) 
    = def concat, x<-(z:zs) 
    elem x (z ++ concat zs)
    = lema elem x (xs ++ ys) = eleme x xs || elem x ys (HAY QUE DEMOSTRARLO PARA VALIDEZ)
    elem x z || elem x (concat zs) 

LEMA 
¿ elem x (xs ++ ys) = eleme x xs || elem x ys?
demostracion rapida! 
caso base ) 
    ys = [] 
    elem x (xs ++ []) = elem x xs || elem x [] 
    = trivial porque elem x [] es el neutro para la disyuncion! 
     lem x (xs) = elem x xs

caso Inductivo) 
    ys = z:zs 
    HI. elem x ( zs ++ xs) = elem x zs || elem x xs 
    TI. ¿ elem x ( (z:zs) ++ xs) = elem x (z:zs) || elem x xs ? 

lado izq ) 
    elem x ( z:zs) 
    = def de elem 2 
     x==z || elem x (zs ++ xs) 

lado der ) 
    elem x (z:zs) || elem x xs 
    = def elem 2 
    x == z || elem x zs || elem x xs 
    = por HI 
    x==z  || elem x ( zs ++ xs) 

    DEMOSTRADO!
-- ------------------------------------------------------

-- e. para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs

 ¿subset xs ys = all (flip elem ys) xs?

caso base ) 
    xs = [] 

lado izq ) 
    subset []  ys  = 
    = def. subset <- [] _ 
    verdadera
    
lado der ) 
    all (flip elem ys) []
    = def all <- 1 
    verdadera

caso Inductivo ) 
    xs = z:zs 
    HI. subset zs  ys = all (flip elem ys) zs 
    TI. ¿subset z:zs  ys = all (flip elem ys) z:zs? 

lado izq ) 
    subset (z:zs)  ys 
    = def. subset <- (z:zs) ys 
    elem z ys && subset zs ys
    = HI 
    elem z ys && all (flip elem ys) zs 
    
lado der ) 
    all (flip elem ys) z:zs
    = def all, x <- flip elem ys (z:zs)
    flip elem ys z &&  all (flip elem ys) zs
    = def flip, x <- elem ys z
    elem z ys && all (flip elem ys) zs

-- ------------------------------------------------------
-- f. all null = null . concat
ppio de ext.
para todo xs 
    ¿all null xs = null  concat xs ? 

caso base) 
    xs = [] 

    all null []= null  (concat [])
    
    ES trivial DA True en ambos casos ! 

caso Inductivo ) 

    xs = z:zs 
    HI. all null zs= null  (concat zs)
    TI. ¿all null z:zs= null  (concat z:zs)?

lado izq ) 
    all null (z:zs )
    = def all, x <- null (z:zs )
    null z && all null zs 
    = HI 
    null z && null  (concat zs)

lado der) 
    null (concat z:zs) 
    = def concat x<- z:zs 
    null (z ++ concat zs) 
    = LEMA null (xs ++ ys) = null xs && null ys 
    null z && null (concat zs) 

LEMA para todo xs, ys 
    ¿ null (xs ++ ys) = null xs && null ys ?

caso 1 xs /= [] && ys =[] 
lado izq 
    null (xs ++ []) 
    = null xs 
lado der 
    null xs && null [] -- null [] es true
    = def null [] 
    null xs 

caso 2 xs =[] ys /= [] 
    llegamos a lo mismo damos por DEMOSTRADO EL LEMA! 
-- ------------------------------------------------------

-- g. length = length . reverse
ppio de ext.
para todo xs 
    ¿ length xs = length (reverse xs)? 

caso base) 
     xs= []
     ¿length [] = length (reverse [])? 
     es truvial en ambos da 0 

caso inductivo 
    xs = z:zs 
    HI. length zs = length (reverse zs) 
    TI. length z:zs = length (reverse z:zs) 


lado izq ) 
    length z:zs 
    = def length, x<- z:zs
    1 + length zs 
lado der ) 
    length (reverse z:zs) 
    = def reverse, x<- z:zs 
    length (reverse zs ++ [z]) 
    = def length, x<- (reverse zs ++ [z])
    length (reverse zs) + length [z] 
    = def length, x<- [z] 
    length (reverse zs) + 1
    = por arit. 
    1 + length (reverse zs)
    = por HI 
    1 + length zs 
    
    QUEDA DEMOSTRADO ! 
-- ------------------------------------------------------

-- h. para todo xs. para todo ys.
-- reverse (xs ++ ys) = reverse ys ++ reverse xs 

caso base ) 
    xs = [] 

    ¿reverse ([] ++ ys) = reverse ys ++ reverse []?
    es trivial !

caso Inductivo ) 
    xs = z:zs 
    HI. reverse (zs ++ ys) = reverse ys + reverse zs 
    TI. reverse ((z:zs) ++ ys) = reverse ys + reverse (z:zs)

lado izq ) 
    reverse ((z:zs) ++ ys)
    = def reverse, x <- ((z:zs) ++ ys)
    reverse (zs ++ ys) ++ [z] 
    reverse ys + reverse zs  ++ [z] 
lado der ) 
    reverse ys + reverse (z:zs)
    = def reverse, x<- z:zs 
    reverse ys ++ reverse zs ++ [z]

    QUEDA DEMOSTRADO!
-- ------------------------------------------------------
-- i. para todo xs. para todo ys.
-- all p (xs++ys) = all p (reverse xs) && all p (reverse ys)

ppio.ext
caso base ) 
   xs = []
   ¿all p ([]++ys) = all p (reverse []) && all p (reverse ys)?

lado izq ) 
    all p ([]++ys)
    = def all 1 
    p[]  && all p ys 
    
    
lado der ) 
    all p (reverse []) && all p (reverse ys)
    = def reverse, x <- [] 
    all p [] && all p (reverse ys)
    all p (reverse ys )
    = por def de reverse. 
    all p ([] ++ reverse ys)
    = def all x<- P ([] ++ reverse ys) 
    p[] && all p (reverse ys)
    = Lema. all p (reverse ys) = all p ys   DEMOSTRAR PARA VALIDAR! 
    Demostrado para caso base! 


caso inductivo) 
    xs = z:zs 
    HI. all p (zs++ys) = all p (reverse zs) && all p (reverse ys)
    TI. ¿all p (z:zs++ys) = all p (reverse z:zs) && all p (reverse ys)?



lado izq ) 
    all p (z:zs++ys)
    = def. all, x<- (z:zs++ys) 
    p(z) && all p (zs++ys)
    = HI 
    p(z) && all p (reverse zs) && all p (reverse ys)
    
lado der ) 
    all p (reverse z:zs) && all p (reverse ys)
    = Lema. all p (reverse ys) = all p ys   DEMOSTRAR PARA VALIDAR! 
    all p (z:zs) && all p (reverse ys)
    = def all, x<- p (z:zs) 
    P(z) && all p (zs) && all p (reverse ys)
    = por lema all p (reverse zs ) = all p zs DEMOSTRAR PARA VALIDAR! 
    p(z) && all (p reverse zs) && all p (reverse ys) 

    Queda medio demostrado! 


-- j. para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)
-- (en este caso, mostrar que no vale)




--                              Sección II

-- b. demostrar las siguientes propiedades:
-- i. para todo n1. para todo n2.
-- evalN (addN n1 n2) = evalN n1 + evalN n2

-- ii. para todo n1. para todo n2.
-- evalN (prodN n1 n2) = evalN n1 * evalN n2

-- iii. int2N . evalN = id

-- iv. evalN . int2N = id 


-- b. demostrar las siguientes propiedades:
-- i. evalNU . succNU = (+1) . evalNU
-- ii. para todo n1. para todo n2.
-- evalNU (addNU n1 n2) = evalNU n1 + evalNU n2
-- iii. nu2n . n2nu = id
-- iv. n2nu . nu2n = id

-- b. demostrar las siguientes propiedades:
-- i. evalNB . normalizarNB = evalNB
-- ii. evalNB . succNB = (+1) . evalNB
-- iii. para todo n1. para todo n2.
-- evalNB (addNB n1 n2) = evalNB n1 + evalNB n2
-- iv. nb2n . n2nb = id
-- v. normalizarNB . normalizarNB = normalizarNB

-- c. solamente una de las siguientes propiedades es verdadera. Dar un
-- contraejemplo para la que no lo sea, y demostrar la que sí lo sea.
-- i. n2nb . nb2n . = id
-- ii. n2nb . nb2n . = normalizarNB


--                                  Sección III

-- i. evalExpA . simplificarExpA = evalExpA
-- ii. cantidadSumaCero . simplificarExpA = const 0

-- b. demostrar por inducción estructural las siguientes propiedades:
-- i. evalExpA . es2ExpA = evalES
-- ii. evalES . expA2es = evalExpA
-- iii. es2ExpA . expA2es = id
-- iv. expA2es . es2ExpA = id
