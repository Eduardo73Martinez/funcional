--dada
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
--a. implementar las siguientes funciones por recursión estructural (o primitiva en
--caso de ser necesario):

evalEA :: EA -> Int
--, que describe el número que resulta de
-- evaluar la cuenta representada por la expresión aritmética dada.
evalEA      (Const n)=  n  
evalEA (BOp op ei ed)= case op of 
                        sum ->  (evalEA ei) + (evalEA ed)  
                        Mul ->  (evalEA ei) * (evalEA ed)  

ea2ExpA :: EA -> ExpA
--, que describe una expresión aritmética
-- representada con el tipo ExpA, cuya estructura y significado son los
-- mismos que la dada.
ea2ExpA      (Const n)   = Cte n 
ea2ExpA (BOp op ei ed)=  operation op (ea2ExpA ei) (ea2ExpA ed) 

operation:: BinOp -> ExpA -> ExpA -> ExpA
operation Sum e1 e2 = Suma e1 e2 
operation Mul e1 e2 = Prod e1 e2 

expA2ea :: ExpA -> EA
-- que describe una expresión aritmética
-- representada con el tipo EA, cuya estructura y significado son los
-- mismos que la dada.
expA2ea   (Cte n )   = Const n
expA2ea  (Suma e1 e2)= BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2 )= BOp Mul (expA2ea e1) (expA2ea e2) 


-- b. demostrar la siguiente propiedad:
-- i. ea2ExpA . expA2ea = id

-- ii. expA2ea . ea2ExpA = id

-- iii. evalExpA . ea2ExpA = evalEA

-- iv. evalEA . expA2ea = evalExpA