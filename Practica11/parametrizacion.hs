

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

succ' = map' (\x-> x+1) 
test' = map (\x-> x==0)


--eje 2 
type Empresa = [Empleado]
data Empleado = E Nombre Sueldo Faltas

type Nombre = String 
type Sueldo = Int   
type Faltas = Int 

sueldo :: Empleado -> Int
sueldo (E n s _) = s 

sueldos :: Empresa -> [Int] 
sueldos = map sueldo 

-- e1 = [E "Edu" 3400, E "Romina" 1800, E "Franco" 2000]
-- e2 = [E "Edu" 340, E "Romina" 180, E "Franco" 200]
-- e3 = [E "Edu" 34, E "Romina" 18, E "Franco" 20]
-- e4 = [E "Edu" 3, E "Romina" 1, E "Franco" 2]


liquidaciones :: [Empresa] -> [[Int]]
liquidaciones = map sueldos 


filter' :: (a->Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs
                        else filter' p xs


e1 = [E "Edu" 3400 4, E "Romina" 1800 10, E "Franco" 2000 0]
e2 = [E "Ma" 3400 0, E "d" 1800 0, E "e" 2000 0]
e3 = [E "d" 3400 3, E "f" 1800 4, E "g" 2000 10]
es = [e1,e2,e3]

ausencias :: Empleado -> Int
ausencias (E _ _ f) = f 

losSarmientos = filter2 (\x->( ausencias x) == 0 ) 
--losSarmientos = filter2 ((== 0) . ausencias) 


sinPresentismos :: [Empresa] -> [Empresa]
sinPresentismos  = filter  ((null ). losSarmientos )


foldr' :: (a -> b ->b) -> b -> [a] -> b
foldr' f g     [] = g
foldr' f g (x:xs) = f x (foldr' f g xs) 
 
map2 f = foldr' (\x acc -> f x : acc) [] 

succ2 = (\x->x+1)


-- filter2 p = foldr' (\x acc ->  if p x then x:acc else acc)  []

elem':: Eq a => a -> [a] ->Bool 
elem' a     [] = False 
elem' a (x:xs) = a==x || elem' a xs 