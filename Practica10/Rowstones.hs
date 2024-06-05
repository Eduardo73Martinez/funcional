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
evalExpRBool       (UnOp uOp exp) t = evalExpRBool exp
evalExpRBool (BinOp op exp1 exp2) t = evalOpBinBool op (evalExpRBool exp1 t)  (evalExpRBool exp2 t) t

evalOpBinBool :: BOp -> Bool-> Bool -> TableroR-> Bool 
evalOpBinBool YTambien x y t  = x && y
evalOpBinBool OBien    x y t= x || y
evalOpBinBool _ _ _ t = boom "operación inválida" t 
-- iii. 
expRTieneTipoInt :: ExpR Int -> Bool
-- , que indica si la
-- expresión dada no falla cuando se ejecuta evalExpRInt.
expRTieneTipoInt = undefined

-- iv. 
expRTieneTipoBool :: ExpR Bool -> Bool
-- , que indica si la
-- expresión dada no falla cuando se ejecuta evalExpRBool.
expRTieneTipoBool = undefined

-- v. 
evalR :: ComandoR -> TableroR -> TableroR
-- , que describe
-- el tablero resultante de evaluar el comando dado en el tablero dado.
evalR = undefined

-- vi. 
cantSacar :: ComandoR -> Int
-- , que describe la cantidad de
-- constructores Sacar que hay en el comando dado.
cantSacar = undefined

-- vii. 
seqN :: Int -> ComandoR -> ComandoR
-- , que describe la
-- secuencia de comandos que reitera el comando dado la cantidad de
-- veces dada.
seqN = undefined

-- viii. 
repeat2Seq :: ComandoR -> ComandoR
-- , que describe el
-- comando resultante de reemplazar, en el comando dado, todas las
-- apariciones del constructor Repetir aplicado a un literal por la
repeat2Seq = undefined
