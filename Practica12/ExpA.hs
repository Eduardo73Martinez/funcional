-- module ExpA(ExpA , cantidadDeCeros , noTieneNegativosExplicitosExpA , simplificarExpA', evalExpA' , showExpA)
--     where 

--Dada la definición de ExpA:


data ExpA = Cte Int | Suma ExpA ExpA  | Prod ExpA ExpA 
    deriving Show 
-- a. Dar el tipo y definir foldExpA, que expresa el esquema de recursión
-- estructural para la estructura ExpA.

-- f = Int -> b 
-- cte::  Int -> ExpA  
-- g = b -> b -> b 
-- suma :: ExpA -> ExpA -> ExpA 
-- h = b -> b -> b 
-- prod :: ExpA -> ExpA -> ExpA 
-- ACORDARSE DEl RESULTADO!                                   -> ExpA -> b 

foldExpA :: (Int -> b ) -> (b -> b -> b ) -> ( b -> b -> b)  -> ExpA -> b 
foldExpA c s p       (Cte n)= c n   
foldExpA c s p (Suma ei ed )= s  (foldExpA c s p ei)  (foldExpA c s p ed)    
foldExpA c s p (Prod ei ed )= p  (foldExpA c s p ei)  (foldExpA c s p ed)


--                      b. Resolver las siguientes funciones utilizando foldExpA:
-- i. 
cantidadDeCeros :: ExpA -> Int
-- , que describe la cantidad de
-- ceros explícitos en la expresión dada.
cantidadDeCeros e =  foldExpA (\n -> if n==0 then 1 else 0 ) 
                    (\ ti td  -> ti + td ) 
                    (\ ti td  -> ti + td )
                    e 
-- ii. 
noTieneNegativosExplicitosExpA :: ExpA -> Bool
-- , que
-- describe si la expresión dada no tiene números negativos de manera
-- explícita.
noTieneNegativosExplicitosExpA e = foldExpA (\n -> if n >= 0 then True else False ) 
                                            (\ ti td  -> ti && td ) 
                                            (\ ti td  -> ti && td )
                                            e 

-- iii. 
simplificarExpA' :: ExpA -> ExpA
-- , que describe una
-- expresión con el mismo significado que la dada, pero que no tiene
-- sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución
-- debe ser exclusivamente simbólica.
simplificarExpA' e  = foldExpA (\n -> Cte n) s  p  e
                            where 
                            s (Cte 0)     td =  td 
                            s ti     (Cte 0) =  ti 
                            p (Cte 1)     td =  td 
                            p ti      (Cte 1)=  ti  


-- iv. 
evalExpA' :: ExpA -> Int
-- , que describe el número que resulta
-- de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' e = foldExpA (\n -> n)  s  p  e 
                        where 
                        s ti td =  ti + td 
                        p ti td =  ti * td 

evalExpA'' :: ExpA -> Int
-- , que describe el número que resulta
-- de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA'' = foldExpA (\n -> n)  (\ti td -> ti + td)  (\ti td -> ti * td)  
    


-- showExpA :: ExpA -> String
-- showExpA (Cte n) = show n
-- showExpA (Suma e1 e2) = "(+" ++ showExpA e1 ++ " " ++ showExpA e2 ++ ")"
-- showExpA (Prod e1 e2) = "(*)" ++ showExpA e1 ++ " " ++ showExpA e

-- v. 
showExpA :: ExpA -> String
-- , que describe el string sin
-- espacios y con paréntesis correspondiente a la expresión dada.
showExpA = foldExpA (\n -> show n)  (\ti td -> "(+" ++ ti ++ td ++ ")" )  (\ti td -> "(*)" ++ ti ++ " " ++ td)  




-- c. Demostrar que evalExpA' es equivalente a evalExpA (ejercicio 6.a.i de la práctica 8).
-- d. Dar el tipo y definir recExpA, que expresa el esquema de recursión primitiva
-- para la estructura ExpA.


recr :: b -> (a->[a]->b->b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)

-- f = Int -> b 
-- cte::  Int -> ExpA  
-- g = b -> b -> b 
-- suma :: ExpA -> ExpA -> ExpA 
-- h = b -> b -> b 
-- prod :: ExpA -> ExpA -> ExpA 
-- ACORDARSE DEl RESULTADO!                                   -> ExpA -> b

recExpA :: b -> (b -> b -> b) -> (b -> b -> b) -> (Int -> b)  -> ExpA -> b 
recExpA z p s y      (Cte n) = z
recExpA z p s y (Suma ei ed) = s (recExpA z p s y ei) (recExpA z p s y ed) 
recExpA z p s y (Suma ei ed) = p (recExpA z p s y ei) (recExpA z p s y ed)   

-- z es la recursion primitiva 
-- y es el caso base 
-- p es la funcion producto, que toma dos expresiones 
-- s es la funcion que suma dos expresiones

-- e. Resolver las siguientes funciones utilizando foldExpA:

-- i. 
cantDeSumaCeros :: ExpA -> Int
-- , que describe la cantidad de
-- constructores de suma con al menos uno de sus hijos constante cero.
cantDeSumaCeros e = foldExpA c s p e
  where
    c _       = 0  -- En otro caso, no se incrementa el contador

    s l r = if l == 0 || r == 0 then 1 else 0  -- Solo se cuenta como suma con cero si alguno de los hijos es cero

    p l r = 0  -- No nos interesan los productos en este contexto, así que siempre retornamos 0


-- ii. 
cantDeProdUnos :: ExpA -> Int
-- , que describe la cantidad de
-- constructores de producto con al menos uno de sus hijos constante
-- uno
cantDeProdUnos e = foldExpA c s p e
                    where 
                    c _ = 0 
                    s l r = 0 
                    p l r = if l == 1 || r == 1 then 1  else 0 

expEjemplo :: ExpA
expEjemplo =
    Suma (Prod (Cte (-1)) (Cte 1))
         (Suma (Cte 1)
               (Prod (Cte 5) (Cte 6)))
