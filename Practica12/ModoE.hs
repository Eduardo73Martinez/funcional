data Nim = Empty -- juego vacío
         | Heap Int Nim -- pila con n fichas (n>0)

type Move = (Int,Int) -- jugada: de la fila i remover k fichas

data GameTree = Nil -- árbol vacío

            | Node (Move, GameTree) -- (jugada, hijos - jugadas del contrincante)
            GameTree -- hermanos (otras jugadas propias)



-- Dar el tipo y la implementación para los esquemas de recursión sobre las estructuras Nim y GameTree
-- (foldNim, recNim, foldGame y recGame respectivamente).

--     g :: b  
-- Empty :: Nim 
--     f :: Int -> b -> b 
-- Heap  :: Int -> Nim -> Nim

-- reusultado :: Nim -> b 

foldNim:: (Int -> b -> b  ) -> b ->  Nim -> b  
foldNim f g Empty = g 
foldNim f g (Heap x td ) =f x (foldNim f g td)

recNim:: (Int ->Nim -> b -> b  ) -> b ->  Nim -> b 
recNim f g      Empty = g 
recNim f g (Heap x td)= f x td (recNim f g td)

-- h  :: b 
-- Nil:: b 

-- f    :: (Move , b) -> b -> b 
-- Node :: (Move , GameTree) -> GameTree -> GameTree

-- resultado :: GameTree -> b 


foldGame :: ((Move, GameTree) -> b -> b) -> b -> GameTree -> b
foldGame f g Nil = g
foldGame f g (Node (c,d) subtree) = f (c,d) (foldGame f g subtree)


recGame :: ((Move, GameTree) ->GameTree ->  b -> b) -> b -> GameTree -> b
recGame f g Nil = g
recGame f g (Node (c,d) subtree) = f (c,d) subtree  (recGame f g subtree)


--Escriba las siguientes funciones sin usar recursión explícita.
heap :: Nim -> Int
--, que indica la cantidad de pilas en un juego de Nim dado.
heap = foldNim (\x xs -> 1 + xs ) 0  

chips :: Nim -> Int
--, que indica la cantidad de fichas en un juego de Nim dado.
chips = foldNim (\x xs ->  x + xs ) 0 

fichas:: Nim -> Int 
fichas (Heap n _) = n 
fichas _ = 0

maxHeap :: Nim -> Int
--, que computa la cantidad de fichas en el heap más grande.
maxHeap        Nim = 0 
maxHeap (Heap n t )=  let elmayor = n 
                        in 
                    if elMayor >= n then  (maxHeap t) else n    

alongside :: Nim -> Nim -> Nim
-- , que une dos juegos de Nim en uno solo que contiene las
-- pilas de ambos en orden. Por ejemplo, el juego de la Figura 1b) puede obtenerse como merge
-- de los juegos de las Figuras 1a) y 2).
alongside = undefined

gameHeight :: GameTree -> Int
-- , que retorna la altura de un GameTree, considerado como
-- árbol general (por ejemplo, el árbol de la Figura 2 tiene altura de juego 3, porque el juego más
-- largo que se puede jugar contiene 3 jugadas).
gameHeight = undefined

branches :: GameTree -> [[Move]]
-- , que retorna la lista de ramas en un GameTree,
-- considerado como árbol general. Una rama es el desarrollo de un juego completo de Nim hasta
-- que haya un ganador
branches = undefined


