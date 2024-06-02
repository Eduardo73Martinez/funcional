
lado izq) 

lado der) 

lado izq) 
    evalExpA (ea2ExpA (Const n))
    =def ea2ExpA , x<- (Const n)
    evalExpA  (operation op (ea2ExpA ei) (ea2ExpA ed)) 

    2 casos------> operation op  =Suma ||  operation op  =Prod 
        caso Suma) 
            evalExpA  (Suma (ea2ExpA ei) (ea2ExpA ed)) 
            = def. evalExpA , x<- Suma (ea2ExpA ei) (ea2ExpA ed)
            evalExpA (ea2ExpA ei) + evalExpA (ea2ExpA ed)
        caso Prod)
            evalExpA  (Prod (ea2ExpA ei) (ea2ExpA ed)) 
            = def. evalExpA , x<- Prod (ea2ExpA ei) (ea2ExpA ed)
            evalExpA (ea2ExpA ei) * evalExpA (ea2ExpA ed)