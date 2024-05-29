
martin = "hola todo bien"

a:: Int-> Int
a x = 0


first:: (Int, Int )-> Int 
first x = fst x

first2 (x,y) = x

apply f = g where g x = f x  
apply2 f x = f(x)
twice2 f x = f (f(x))
doble x = f where f = x + x 

swap (x ,y) = (y, x)

uflip f = g where g p = f (swap p)

twice f = g where g x = f (f(x))

const x = g where  g y = x

appDup f = g where g x = f(x,x)

appDist f = g where g (x, y) = (f x, f y)

suma:: (Int, Int) ->Int 
suma (a, b)= a + b

appFork (f,g) = h where h x = (f(x), g(x))

flip f = h where h x = k where k y = f y x

flip2 f x y = f y x
{--


b. 1 +  first
g. (twice doble) doble
h. (twice twice) first
i. apply applyif 3 < 5 then 3 else 5
c. let par = (True, 4)
in (if first par then first par else second par)
d. (doble doble) 5
e. doble (doble 5)
f. twice
--}


subst f = h where h g = k where k x = (f x) (g x) 

miF = \p -> let (f, g) = p in \x -> (f x, g x)

mif' = \f -> (\px -> let (x, y) = px in (f x, f y))

mif'' = \pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, g y)

appPar (f, g) = h where h (x, y) = (f x, g y)

uflip2 f p = f (swap p)

f2 = \f -> (\x -> f (x, x))

compose =(\f -> (\g -> (\x -> f(g(x)) ) ))

b = uncurry (curry (snd))




 ---- ------------------------PRACTICA 4 ----------------------------------------

udiv (x, 0) = error "no puede dividir por 0"

udiv2 (x, y) = div x y 

udivH = uncurry div 

succ3 x = x + 1 

succH = succ3 1 

porLaMitad = flip2 div 2 

conDieresis:: Char -> String 
conDieresis 'u'= "ü"

conDieresisB 'u' = "ü" 

conDieresisC c = conDieresisB c 

conTildePM 'a' = "á" 
 


esVocal:: Char -> Bool 
esVocal 'a' = True 
esVocal 'e' = True 
esVocal 'i' = True 
esVocal 'o' = True 
esVocal 'u' = True 
esVocal _ = False  

conTildeE :: Char -> String 
conTildeE c = if (esVocal c) 
                then conTildePM c 
                else error "no es una tilde"
