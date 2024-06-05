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

evalOpUn :: UOp -> a -> a  
evalOpUn        No x = undefined
evalOpUn Siguiente x = undefined
evalOpUn    Previo x = undefined

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
-- , que describe la
-- secuencia de comandos que reitera el comando dado la cantidad de
-- veces dada.
seqN          (Mover d) = 0
seqN            (Poner) = 0
seqN            (Sacar) = 1 
seqN             (NoOp) = 0 
seqN (Repetir (expN) c) = cantSacar c                
seqN  (Mientras expB c) = cantSacar c    
seqN  (Secuencia c1 c2) = cantSacar c1 +  cantSacar c2      

-- viii. 
repeat2Seq :: ComandoR -> ComandoR
-- , que describe el
-- comando resultante de reemplazar, en el comando dado, todas las
-- apariciones del constructor Repetir aplicado a un literal por la
repeat2Seq = undefined
