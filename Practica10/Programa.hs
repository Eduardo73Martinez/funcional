import Memoria 
import NExp
import BExp 

data Programa = Prog Bloque
type Bloque = [Comando]
data Comando = Assign Nombre NExp | If BExp Bloque Bloque | While BExp Bloque

type Nombre = String

-- ○ implementar las siguientes funciones:
evalProg :: Programa -> Memoria -> Memoria
-- , que
-- describe la memoria resultante de evaluar el programa dado a partir
-- de la memoria dada.
evalProg (Prog ins) m = evalBlq ins m 


evalBlq :: Bloque -> Memoria -> Memoria
-- , que describe la
-- memoria resultante de evaluar el bloque dado a partir de la memoria
-- dada.
evalBlq     [] m = m
evalBlq (i:is) m =  evalBlq is m' 
                    where m' = evalCom i m 


evalCom :: Comando -> Memoria -> Memoria
-- , que describe
-- la memoria resultante de evaluar el comando dado a partir de la
-- memoria dada.
evalCom  (Assign nomVar n) m = recordar nomVar (evalNExp n m) m
evalCom (If boo bl1 bl2)   m = if evalBExp boo m
                                    then  evalBlq  bl1 m
                                    else  evalBlq  bl2 m
evalCom   (While be bl)    m =  evalCom (If be  (bl ++ bucle) corte) m
                                where 
                                --condicion = evalBExp be m
                                bucle     = [While be bl]
                                corte     = []


optimizeCF :: Programa -> Programa
optimizeCF  (Prog bls) = Prog (optimizeBls bls )

optimizeBls:: Bloque -> Bloque 
optimizeBls     [] =  []
optimizeBls (x:xs) = optimizeComando x :  optimizeBls xs

optimizeComando :: Comando  -> Comando
optimizeComando  (Assign nomVar n) = (Assign nomVar n) 
optimizeComando   (If boo bl1 bl2) =  If boo (optimizeBls bl1) (optimizeBls bl2 )
optimizeComando      (While be bl) = While be (optimizeBls bl) 





demostrar las siguientes propiedades:
i.  para todo c. para todo cs.
    evalBloque (c:cs) = evalBloque cs . evalComando c

    aplicamos p e 
    evalBloque (c:cs) m = evalBloque cs . evalComando c m 

    por def. (.) es equivalente a 
    evalBloque (c:cs) m = evalBloque cs (evalComando c m) 

DEMOSTRACION)
    evalBloque (c:cs) m 
    = por definicion de evalBloque , x <- (c:cs) m 
    evalBloque cs (evalComando c m) 
    DEMOSTRADO! 
---------------------------------------------------------------------------------------------------------------------------

ii. Para todo cs1. para todo cs2.
    evalBloque (cs1++cs2)= evalBloque cs2 . evalBloque cs1

    aplicamos p e 
    evalBloque (cs1++cs2) mem = evalBloque cs2 . evalBloque cs1 mem 

    por def. (.) es equivalente a 
    evalBloque (cs1++cs2) mem = evalBloque cs2 (evalBloque cs1 mem )

    vamos a demostrar por induccion estructural!

DEMOSTRACION ! 

CASO BASE ) 
    cs1 = []
    cs2 = []

lado izq) 
    evalBloque ([]) mem
    = def evalBloque
    mem

lado der) 
    evalBloque [] (evalBloque [] mem )
                                         = def evalBloque
    evalBloque [] (mem)
                                        = def evalBloque 
    mem

CASO INDUCTIVO)     
    cs1 = x:xs 

    HI. evalBloque (x:cs2) mem = evalBloque cs2 (evalBloque x mem ) 
    TI.¿ evalBloque (x:xs ++ cs2) mem = evalBloque cs2 (evalBloque (x:xs ++ cs2)  mem ) ? 

lado izq ) 
     evalBloque (x:xs ++ cs2) mem  
                                        = por def evalBloque, x <- (x:xs ++ cs2) mem  
    evalBloque (xs++cs2) (evalComando x m) 
    = def 



lado der ) 
    evalBloque cs2 (evalBloque x:xs mem ) 
     
     PREGUNTAR CUANTOS CASOS RECURSIVOS TIENE Y SOBRE QUE ESTRUCTURA RECORRER!  NO SALIO 


-- iii. Para todo be. para todo cs1. para todo cs2.
--     evalComando (If be cs1 cs2) = evalComando (If (Not be) cs2 cs1)
--     por p. e, ¿ evalComando (If be cs1 cs2) mem = evalComando (If (Not be) cs2 cs1) mem ?

-- DEMOSTRACION 
--     evalComando (If (Not be) cs2 cs1) mem
--         = por def evalComando, x <- (If (Not be) cs2 cs1) mem 
--             if evalBExp (Not be) m then  evalBlq cs2 m else  evalBlq  cs1 m
--         = por def evalBExp , x<- (Not be) m 
--             if  not (evalBExp b m) then  evalBlq cs2 m else  evalBlq  cs1 m
--         = por definicion de "if" debemos invertir las ramas del "then" y el "else" si sacamos el not 
--             if (evalBExp b m) then  evalBlq cs1 m else  evalBlq  cs2 m 
--         = por def evalComando 
--         evalComando (If be cs1 cs2) mem 
--         DEMOSTRADO! 

-- iv. para todo x. para todo ne1. para todo ne2.
-- si (para todo mem. evalNExp ne2 (recordar x v mem)= evalNExp ne2 mem) entonces evalBloque [Assign x ne1, Assign x ne2] = evalComando (Assign x ne2)
-- AYUDA: el antecedente solamente establece que x no aparece en ne2 y por lo tanto no influye en el resultado


-- DEMOSTRACION 
--     evalBloque [Assign x ne1, Assign x ne2] = evalComando (Assign x ne2)
