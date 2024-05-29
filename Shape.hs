module Shape(Shape, construyeShNormal) where

import Helado
     
data Shape = Circle Float | Rect Float Float 

construyeShNormal:: (Float -> Shape) -> Shape 
construyeShNormal c = c 1.0 

swap (x ,y) = (y, x)
data Maybe a = Just a | Nothing
compose =(\f -> (\g -> (\x -> f(g(x)) ) ))