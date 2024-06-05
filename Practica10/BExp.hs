module BExp(BExp, evalBExp, cfBExp' )
    where 
import Memoria 

--                                      NECESITO USAR LOS CONSTRUCTORES DE NEXP

data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp 
    deriving Show 

data NBinOp = Add | Sub | Mul | Div | Mod | Pow 
    deriving Show 
type Variable = String
    
-------------------------------------------------------------
data BExp = BCte Bool | Not BExp | And BExp BExp| Or BExp BExp| ROp RelOp NExp NExp

data RelOp = Eq | NEq -- Equal y NotEqual
            | Gt | GEq -- Greater y GreaterOrEqual
            | Lt | LEq -- Lower y LowerOrEqual

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


-- ○ implementar las siguientes funciones:
evalBExp :: BExp -> Memoria -> Bool
--, que describe el
-- booleano que resulta de evaluar la expresión dada a partir de la
-- memoria dada.
evalBExp           (BCte b) m = b 
evalBExp           (Not b ) m = not (evalBExp b m) 
evalBExp        (And bi bd) m = (evalBExp bi m) && (evalBExp bd m)
evalBExp         (Or bi bd) m = (evalBExp bi m) || (evalBExp bd m)
evalBExp    (ROp rop bi bd) m = evalROp rop (evalNExp bi m) (evalNExp bd m)

evalROp:: RelOp -> Int -> Int -> Bool
evalROp  Eq  = (==)
evalROp  NEq = (/=)
evalROp  Gt  = (>)
evalROp  GEq = (>=)
evalROp  Lt  = (<)
evalROp  LEq = (<=)

cfBExp' :: BExp -> BExp                                                          --MI IMPLEMENTACION
--, que describe una expresión con el
-- mismo significado que la dada, pero reemplazando las
-- subexpresiones que no dependan de la memoria por su expresión
-- más sencilla. La resolución debe ser exclusivamente simbólica.
-- ○ demostrar la siguiente propiedad:
cfBExp'           (BCte b) = (BCte b)
cfBExp'           (Not b ) = Not (cfBExp' b)
cfBExp'        (And bi bd) = And (cfBExp'  bi) (cfBExp' bd)
cfBExp'         (Or bi bd) = Or  (cfBExp'  bi) (cfBExp' bd)
cfBExp'         (ROp rop bi bd) = error "TypeError: Esta ejecucion depende de la memoria " 
--cfBExp    (ROp rop bi bd) = evalROp rop (evalNExp  bi) (evalNExp bd)          --NO tiene este caso porque usa la mem! 


cfBExp :: BExp -> BExp                                                  --IMPLEMETACION FIDEL
cfBExp          (BCte b) = BCte b
cfBExp          (Not be) = cfNot (cfBExp be)
cfBExp     (And be1 be2) = cfAnd (cfBExp be1) (cfBExp be2)
cfBExp      (Or be1 be2) = cfOr (cfBExp be1) (cfBExp be2)
cfBExp (ROp rop ne1 ne2) = cfROp rop (cfNExp ne1)  (cfNExp ne2)

cfNot :: BExp -> BExp
cfNot (BCte b) = BCte (not b)
cfNot be = Not be

cfAnd :: BExp -> BExp -> BExp
cfAnd (BCte b) be2 = if b then be2 else BCte False
cfAnd be1 be2 = And be1 be2 

cfOr :: BExp -> BExp -> BExp
cfOr (BCte b) be2 = if b then BCte True else be2
cfOr be1 be2 = Or be1 be2

cfROp :: RelOp -> NExp -> NExp -> BExp
cfROp rop (NCte x) (NCte y) = BCte (evalROp rop x y)
cfROp rop ne1 ne2 = ROp rop ne1 ne2           


--evalBExp . cfBExp = evalBExp
--         = PPE 
--         evalBExp . cfBExp x = evalBExp x  
--         por def (.)
--         evalBExp (cfBExp x)  = evalBExp x  

--         para todo x, ¿ evalBExp (cfBExp x)  = evalBExp x?

-- caso base ) 
--         x = (BCte b) 
        

-- lado izq ) 
--     evalBExp (cfBExp (BCte b) )
--     = def cfBExp 
--     evalBExp (BCte b)
--     = def. evalBExp
--      b 
-- lado der)   
--     evalBExp (BCte b)
--     = def. evalBExp
--     b 
--     demostrado! 

-- caso inductivo 1 ) 
--      x = (Not be)
--     HI. evalBExp (cfBExp be) m = evalBExp be m 
--     TI. ¿evalBExp (cfBExp (Not be)) m = evalBExp (Not be) m?

-- lado izq ) 
--     evalBExp (cfBExp (Not be)) m 
--     = def cfBExp 
--     evalBExp (cfNot (cfBExp be)) m
--     = def cfNot 
--     evalBExp (Not cfBExp be) m 
--     = por def evalBExp
--     not evalBExp (cfBExp be) m 
--     = por HI 
--     not evalBExp be m 
    
-- -- lado der)   
--     evalBExp (Not be) m
--     = por def evalBExp
--     not evalBExp (cfBExp be) m 

--     DEMOSTRADO! 
