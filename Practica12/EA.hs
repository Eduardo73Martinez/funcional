module EA(EA, noTieneNegativosExplicitosEA, simplificarEA', evalEA', showEA , ea2ExpA', ea2Arbol' ) 
    where


data ExpA = Cte Int | Suma ExpA ExpA  | Prod ExpA ExpA 


--Ejercicio 2) Dada la definición de EA:
--a. Dar el tipo y definir foldEA, que expresa el esquema de recursión estructural
--para la estructura EA.

-- f = Int -> b 
-- Const::  Int -> EA 
-- g = BinOp -> b -> b -> b 
-- BOp:: BinOp -> EA -> EA -> EA   

--resultado = EA -> b 

foldEA :: (Int -> b ) -> (BinOp -> b -> b -> b ) -> EA -> b 
foldEA f g   (Const n) = f n 
foldEA f g (BOp bp ei ed) = g bp (foldEA f g ei) (foldEA f g ed)  

--b. Resolver las siguientes funciones utilizando foldEA:
--i. 
noTieneNegativosExplicitosEA :: EA -> Bool
-- , que
-- describe si la expresión dada no tiene números negativos de manera
-- explícita.
noTieneNegativosExplicitosEA e = foldEA (\n-> n>0 ) (\b x y -> x && y ) e 


-- ii. 
simplificarEA' :: EA -> EA
-- , que describe una expresión con
-- el mismo significado que la dada, pero que no tiene sumas del
-- número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser
-- exclusivamente simbólica.
simplificarEA' e  = foldEA (\n -> Const n ) g e 
                    where 
                    g Sum (Const 0) ed = ed 
                    g Sum ti (Const 0) = ti 
                    g Mul ti (Const 1) = ti 
                    g Mul (Const 1) td = td 

-- iii. 
evalEA' :: EA -> Int
-- , que describe el número que resulta de
-- evaluar la cuenta representada por la expresión aritmética dada.
evalEA' e = foldEA (\n ->n) (\b x y ->if esIgual b Mul then x * y else x + y ) e

esIgual :: BinOp -> BinOp -> Bool
esIgual Mul Mul = True
esIgual Sum Sum = True  
esIgual  _  _   = False


-- showExpA :: ExpA -> String
-- showExpA (Cte n) = show n
-- showExpA (Suma e1 e2) = "(+" ++ showExpA e1 ++ " " ++ showExpA e2 ++ ")"
-- showExpA (Prod e1 e2) = "(*)" ++ showExpA e1 ++ " " ++ showExpA e

-- iv. 
showEA :: EA -> String
-- , que describe el string sin espacios y
-- con paréntesis correspondiente a la expresión dada.
showEA e = foldEA (\n -> show n) (\ b ti td -> case  b of 
                                            Sum -> "(+" ++ ti ++ " " ++ td ++ ")" 
                                            Mul -> "(*)" ++ ti ++ " " ++ td )  e 

 
-- v. 
ea2ExpA' :: EA -> ExpA
-- , que describe una expresión aritmética
-- representada con el tipo ExpA, cuyo significado es el mismo que la
-- dada.
ea2ExpA' e  = foldEA (\n-> Cte n) (\b x y -> if esIgual Mul b then Prod x y else Suma x y) e 


data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul
data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b)

-- vi. 
ea2Arbol' :: EA -> ABTree BinOp Int 
--, que describe la
-- representación como elemento del tipo ABTree BinOp Int de la
-- expresión aritmética dada.
ea2Arbol' ea  = foldEA (\n ->Leaf n ) (\b x y->Node b x y ) ea





-- c. Demostrar que evalEA' es equivalente a evalEA (ejercicio 1.a.i de la
-- práctica 9)