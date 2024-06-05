
PREGUNTAS PARA EL JUEVES ! 


PRACTICA 10 

EJERCICIO N°3 

ii. para todo cs1. para todo cs2.
evalBloque (cs1++cs2)
= evalBloque cs2 . evalBloque cs

no salió la demostracion 


optimizeCF :: Programa -> Programa
no sale la implementacion 



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

iv. para todo x. para todo ne1. para todo ne2.
si (para todo mem. evalNExp ne2 (recordar x v mem)= evalNExp ne2 mem) entonces evalBloque [Assign x ne1, Assign x ne2] = evalComando (Assign x ne2)
AYUDA: el antecedente solamente establece que x no aparece en ne2 y por lo tanto no influye en el resultado


DEMOSTRACION 
    evalBloque [Assign x ne1, Assign x ne2] mem 
    = def evalBloque 

    evalBloque [Assign x ne2] ( evalCom (Assign x ne1) mem)

    = def evalCom 
    evalBloque [Assign x ne2]   (recordar x (evalNExp ne1 mem) mem )

    = def evalBloque 
    evalBloque [] ( evalCom (Assign x ne2) (recordar x (evalNExp ne1 mem) mem )) 

    =por def evalCom 
    evalCom (Assign x ne2) (recordar x (evalNExp ne1 mem) mem )

    llamo m' = (recordar x (evalNExp ne1 mem) mem ) 

    evalCom (Assign x ne2) m'
    = def evalcom 

    evalCom (Assign x ne2) m'

    recordar x (evalNExp ne2 m') m'

    
