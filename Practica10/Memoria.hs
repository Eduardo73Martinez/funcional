module Memoria(Memoria, enBlanco, cuantoVale, recordar, variables )
    where 

data Memoria = A

type Variable = String

enBlanco :: Memoria
-- , que describe una memoria vacía.
enBlanco =  undefined

cuantoVale :: Variable -> Memoria -> Maybe Int
-- , que describe
-- el número asociado a la variable dada en la memoria dada.
cuantoVale = undefined

recordar :: Variable -> Int -> Memoria -> Memoria
-- , que la
-- memoria resultante de asociar el número dado a la variable dada en la
-- memoria dada.
recordar =  undefined

variables :: Memoria -> [Variable]
-- , que describe las variables que
-- la memoria recuerda
variables = undefined