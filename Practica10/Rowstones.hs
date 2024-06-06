import TableroR

data UOp = No | Siguiente | Previo

data BOp = YTambien | OBien | Mas | Por

type ProgramaR = ComandoR

data ComandoR = Mover DirR | Poner| Sacar| NoOp| Repetir (ExpR Int) ComandoR | Mientras (ExpR Bool) ComandoR 
                | Secuencia ComandoR ComandoR

data ExpR a = Lit a | PuedeMover DirR | NroBolitas | HayBolitas | UnOp  UOp (ExpR a) | BinOp BOp (ExpR a) (ExpR a)
--Ejercicio 4) Dada la siguiente representación del lenguaje Rowbstones:
-- implementar las siguientes funciones:
-- i. 
evalExpRInt :: ExpR Int -> TableroR -> Int
-- , que
-- describe el número que resulta de evaluar la expresión dada en el
-- tablero dado.
-- NOTA: en caso de que la expresión no pueda computar un número,
-- debe ser considerada una operación inválida.
evalExpRInt              (Lit x) t = x
evalExpRInt       (PuedeMover d) t = boom "operación inválida " t 
evalExpRInt         (NroBolitas) t = nroBolitas t 
evalExpRInt         (HayBolitas) t = boom "operación inválida " t 
evalExpRInt       (UnOp uOp exp) t = evalExpRInt exp t
evalExpRInt (BinOp op exp1 exp2) t = evalOpBinInt op (evalExpRInt exp1 t)  (evalExpRInt exp2 t)  t

evalOpBinInt :: BOp -> Int-> Int ->TableroR -> Int
evalOpBinInt Mas x y t= x + y 
evalOpBinInt Por x y t= x * y 
evalOpBinInt _   _ _ t= boom "operación inválida " t 


-- ii. 
evalExpRBool :: ExpR Bool -> TableroR -> Bool
-- , que
-- describe el booleano que resulta de evaluar la expresión dada en el
-- tablero dado.
-- NOTA: en caso de que la expresión no pueda computar un booleano,
-- debe ser considerada una operación inválida.
evalExpRBool              (Lit x) t = x 
evalExpRBool       (PuedeMover d) t = puedeMover d t 
evalExpRBool         (NroBolitas) t = boom "operación inválida" t 
evalExpRBool         (HayBolitas) t = hayBolitas t 
evalExpRBool       (UnOp uOp exp) t = evalExpRBool exp t 
evalExpRBool (BinOp op exp1 exp2) t = evalOpBinBool op (evalExpRBool exp1 t)  (evalExpRBool exp2 t) t

evalOpBinBool :: BOp -> Bool-> Bool -> TableroR-> Bool 
evalOpBinBool YTambien x y t  = x && y
evalOpBinBool OBien    x y t= x || y
evalOpBinBool _ _ _ t = boom "operación inválida" t 

-- iii. 
expRTieneTipoInt :: ExpR Int -> Bool
-- , que indica si la
-- expresión dada no falla cuando se ejecuta evalExpRInt.
expRTieneTipoInt              (Lit x) = True
expRTieneTipoInt       (PuedeMover d) = False
expRTieneTipoInt         (NroBolitas) = True  
expRTieneTipoInt         (HayBolitas) = False 
expRTieneTipoInt       (UnOp uOp exp) = False
expRTieneTipoInt (BinOp op exp1 exp2) =  isTypeBinOPInt op (exp1)  (exp2) 
                                    

isTypeBinOPInt :: BOp -> ExpR Int-> ExpR Int -> Bool 
isTypeBinOPInt Mas x y = expRTieneTipoInt x  && expRTieneTipoInt y
isTypeBinOPInt Por x y = expRTieneTipoInt x  && expRTieneTipoInt y
isTypeBinOPInt _   _ _ = False 

-- iv. 
expRTieneTipoBool :: ExpR Bool -> Bool
-- , que indica si la
-- expresión dada no falla cuando se ejecuta evalExpRBool.
expRTieneTipoBool              (Lit x) = True
expRTieneTipoBool       (PuedeMover d) = False
expRTieneTipoBool         (NroBolitas) = True  
expRTieneTipoBool         (HayBolitas) = False 
expRTieneTipoBool       (UnOp uOp exp) = False
expRTieneTipoBool (BinOp op exp1 exp2) =  isTypeBinOPBool op (exp1)  (exp2) 
                                    

isTypeBinOPBool :: BOp -> ExpR Bool-> ExpR Bool -> Bool 
isTypeBinOPBool OBien    x y = expRTieneTipoBool x  && expRTieneTipoBool y
isTypeBinOPBool YTambien x y = expRTieneTipoBool x  && expRTieneTipoBool y
isTypeBinOPBool        _ _ _ = False 

-- v. 
evalR :: ComandoR -> TableroR -> TableroR
-- , que describe
-- el tablero resultante de evaluar el comando dado en el tablero dado.
evalR         (Mover d) t = mover d t 
evalR           (Poner) t = poner t 
evalR           (Sacar) t = sacar t 
evalR            (NoOp) t = boom "no se puede realizar ninguna operacion" t
evalR  (Repetir (Lit n) c) t = if  n >= 0 
                                then evalR (Repetir (Lit (n-1)) c ) t
                                else t 
                                
evalR (Mientras expB c) t = if  bool
                                then evalR (Mientras expB  c ) t
                                else t  
                                where bool = evalExpRBool expB t  --- ¿no hay un loop infinito aca?
evalR (Secuencia c1 c2) t = evalR c1 (evalR c2 t)          

-- vi. 
cantSacar :: ComandoR -> Int
-- , que describe la cantidad de
-- constructores Sacar que hay en el comando dado.
cantSacar         (Mover d) = 0
cantSacar           (Poner) = 0
cantSacar           (Sacar) = 1 
cantSacar            (NoOp) = 0 
cantSacar(Repetir (expN) c) = cantSacar c                
cantSacar (Mientras expB c) = cantSacar c    
cantSacar (Secuencia c1 c2) = cantSacar c1 +  cantSacar c2      

-- vii. 
seqN :: Int -> ComandoR -> ComandoR
-- que describe la
-- secuencia de comandos que reitera el comando dado la cantidad de
-- veces dada
seqN 0 c = c 
seqN n c = Repetir  (Lit (n-1))   (seqN (n-1) c)

-- viii. 
repeat2Seq :: ComandoR -> ComandoR
-- , que describe el
-- comando resultante de reemplazar, en el comando dado, todas las
-- apariciones del constructor Repetir aplicado a un literal por la
-- secuencia de comandos que reitera el comando correspondiente la
-- cantidad de veces dada por el literal
repeat2Seq NoOp = NoOp
repeat2Seq (Mover dir) = Mover dir
repeat2Seq Poner = Poner
repeat2Seq Sacar = Sacar
repeat2Seq (Repetir (Lit n) cmd') = seqN n cmd'          -- Reemplazamos el Repetir por una secuencia de comandos repetidos n veces
repeat2Seq (Repetir exp cmd') = Repetir exp (repeat2Seq cmd')   
repeat2Seq (Mientras exp cmd') = Mientras exp (repeat2Seq cmd')
repeat2Seq (Secuencia cmd1 cmd2) = Secuencia (repeat2Seq cmd1) (repeat2Seq cmd2)



-----------------------------------------------------------------------------------
sequence :: [(a -> a)] -> a -> a
sequence [] x = x
sequence (f:fs) x = f (sequence fs x)

evalMany :: Int -> ProgramaR -> TableroR -> TableroR
evalMany 0 p t = t
evalMany n p t = evalR p (evalMany (n-1) p t)

evalList :: ProgramaR -> [TableroR] -> [TableroR]
evalList p [] = []
evalList p (t:ts) = evalR p t : evalList p ts


para todo p. para todo ts1. para todo ts2.
¿ evalList p (ts1 ++ ts2) = evalList p ts1 ++ evalList p ts2?

    vamos a demostrar por induccion estructural  
caso base ) 
    ts1 = []
    ts2 = []

lado izq) 
    evalList p ([]) 
    = por def evalList
    [] 

lado der)  
    evalList p [] ++ evalList p []
    = por def evalList
    [] 

caso inductivo) 
    ts1 = x:xs
    ts2 = y:ys 
    HI.evalList p ([x] ++ ts2) = evalList p [x] ++ evalList p ts2
    TI. ¿evalList p (x:xs ++ ts2) = evalList p x:xs ++ evalList p ts2?

lado izq) 
    evalList p (x:xs ++ ts2)
    = por def evalList
    evalR p x : evalList p (xs++ts2)

lado der) 
    evalList p x:xs ++ evalList p ts2 

    = por def evalList  , x <- p x:xs

    evalR p x : evalList p (xs) ++ evalList p ts2  

------------------------------------------------------------------------------------------------------

para todo n.
    eval (Repetir (Lit n) Poner) = eval (seqN n Poner)  
    
    =por princio de extensionabilida

   ¿ eval (Repetir (Lit n) Poner) t = eval (seqN n Poner)  t ? 


lado der) 
    eval (seqN n Poner)  t
    = def seqN 
    Repetir  (Lit (n-1))   (seqN (n-1) Poner)
    
    