module Helado(Helado, chocoHelate) where 

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon  
    deriving Show 

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto  Gusto
    deriving Show 



chocoHelate constH = constH Chocolate 
compose =(\f -> (\g -> (\x -> f(g(x)) ) ))

