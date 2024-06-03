module NExp(NExp,evalNExp, cfNExp )
    where 
import Memoria 


data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp

data NBinOp = Add | Sub | Mul | Div | Mod | Pow

type Variable = String

-------------------------------------------------------------

evalNExp :: NExp -> Memoria -> Int
-- , que describe el
-- número resultante de evaluar la expresión dada a partir de la memoria
-- dada.
evalNExp        (NCte n) mem =  n 
evalNExp         (Var x) mem =  case cuantoVale x mem of 
                                Nothing -> error "TypeError: Variable no definida en mem"
                                Just v  -> v 
evalNExp (NBOp op e1 e2) mem =  evalNBOp  op  (evalNExp e1 mem)  (evalNExp e2 mem)


evalNBOp :: NBinOp -> (Int -> Int -> Int)
evalNBOp Add = (+)
evalNBOp Sub = (-)
evalNBOp Mul = (*)
evalNBOp Div = div
evalNBOp Mod = mod
evalNBOp Pow = (^)

cfNExp :: NExp -> NExp
-- , que describe una expresión con el
-- mismo significado que la dada, pero simplificada y reemplazando las
-- subexpresiones que no dependan de la memoria por su expresión
-- más sencilla. La resolución debe ser exclusivamente  simbólica
cfNExp                     (Var x) =  Var x 
cfNExp                    (NCte n) = (NCte n)
cfNExp (NBOp op (NCte x) (NCte y)) =  NCte (evalNBOp op x y)            -- HAGO DOBLE PATER MACHING! AGREGO UN CASO.
cfNExp             (NBOp op e1 e2) =  NBOp op (cfNExp e1) (cfNExp e2)


-- demostrar! 
-- evalNExp . cfNExp = evalNExp 

--     ppe 
--     evalNExp . cfNExp x = evalNExp  x 

--     por def (.)
    
--     evalNExp (cfNExp x) m= evalNExp  x m
--     para todo x, vale ¿ evalNExp (cfNExp x) m= evalNExp  x m ? 
    
--     vamos a demostrar por induccion estructural! 

-- caso base 1) 
--     x = (Var x') 

-- lado izq) 
--     evalNExp (cfNExp (Var x') ) m
--     = def cfNExp 
--     evalNExp ( Var x'  ) m
-- lado der) 
--     evalNExp ( Var x'  ) m
--     demostrado! 

-- caso base 2) 
--     x = (NCte n)

-- lado izq) 
--     evalNExp (cfNExp (NCte n) ) m
--     = def cfNExp 
--     evalNExp (NCte n) m
-- lado der) 
--     evalNExp (NCte n) m
--     demostrado! 


-- caso inductivo) 
--     x = (NBOp op e1 e2)
--     HI1. evalNExp (cfNExp e1)  m = evalNExp  e1 m
--     HI1. evalNExp (cfNExp e2)  m = evalNExp  e2 m
--     TI.¿ evalNExp (cfNExp (NBOp op e1 e2)) m = evalNExp  (NBOp op e1 e2) m?

-- lado izq) 
--     evalNExp (cfNExp (NBOp op e1 e2)) m
--     = def cfNExp 2 
--     evalNExp (NBOp op (cfNExp e1) (cfNExp e2)) m 
--     = def evalNExp

--     evalNBOp  op  (evalNExp (cfNExp e1) mem)  (evalNExp (cfNExp e2) mem)
--     = HI1 e HI2 
--     evalNBOp  op  ( evalNExp  e1 m)  (evalNExp  e2 m)

-- lado der) 
--     evalNExp  (NBOp op e1 e2) m
--     = def evalNExp 2 
--     evalNBOp  op  ( evalNExp  e1 m)  (evalNExp  e2 m)
--     DEMOSTRADO!