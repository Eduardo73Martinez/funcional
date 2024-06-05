
module TableroR(TableroR, mover, poner, sacar, nroBolitas, hayBolitas, puedeMover,boom,DirR)
     where 

data DirR = Oeste | Este 
data TableroR = A

tableroInicial :: Int -> TableroR
--, que describe un tablero inicial.
tableroInicial = undefined

mover :: DirR -> TableroR -> TableroR
-- , que describe el tablero a
-- partir del tablero dado donde el cabezal se movió hacia la dirección dada.
-- Esta operación es parcial, pues debe poderse mover en la dirección dada (ver
-- puedeMover).
mover = undefined

poner :: TableroR -> TableroR
-- , que describe el tablero donde se
-- agregó una bolita de la celda actual del tablero dado.
poner = undefined

sacar :: TableroR -> TableroR
--, que describe el tablero donde se
-- sacó una bolita de la celda actual del tablero dado. Esta operación es parcial,
-- pues debe haber bolitas en la celda actual (ver hayBolitas).
sacar = undefined

nroBolitas :: TableroR -> Int
-- , que describe la cantidad de bolitas
-- en la celda actual del tablero dado.
nroBolitas = undefined

hayBolitas :: TableroR -> Bool
-- , que indica si hay bolitas en la celda
-- actual del tablero dado.
hayBolitas = undefined

puedeMover :: DirR -> TableroR -> Bool
-- , que indica si el cabezal
-- se puede mover hacia la dirección dada en el tablero dado.
puedeMover = undefined

boom :: String -> TableroR -> a
-- , que describe el resultado de
-- realizar una operación de consulta inválida sobre un tablero
boom = undefined