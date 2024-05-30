--                                      SECCION 1  

-- Ejercicio 4) Demostrar las siguientes propiedades: 
-- a. cantidadDeAceitunas Prepizza 
--   ​=​ cantidadDeAceitunas(conAceitunasJuntas Prepizza) 

{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeAceitunas Prepizza   
        aplicamos DEF. cantidadDeAceitunas. x<-prepizza 

    0                              
        obtenemos como resultado "0"


LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeAceitunas(conAceitunasJuntas Prepizza) 
        aplicamos Def. conAceitunasJuntas. x<- Prepizza 

    cantidadDeAceitunas (Prepizza)                  
        aplicamos DEF. cantidadDeAceitunas. x<-prepizza

    0                                                
        obtenemos como resultado "0"

    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!

--}


-- b. cantidadDeAceitunas (Capa Queso Prepizza) 
--   ​=​ cantidadDeAceitunas 
--       (conAceitunasJuntas (Capa Queso Prepizza)) 

{-- 
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeAceitunas (Capa Queso Prepizza)  
        aplicamos Def. cantidadDeAceitunas. x<- Capa Queso Prepizza 

    0 
        obtenemos como resultado "0"

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeAceitunas (conAceitunasJuntas (Capa Queso Prepizza)) 
        aplicamos Def. conAceitunasJuntas.  x <- Capa Queso Prepizza 

    cantidadDeAceitunas (Capa Queso Prepizza )
        aplicamos Def. cantidadDeAceitunas. x<- Capa Queso Prepizza 

    0 
        obtenemos como resultado "0"

    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!

--}



-- c. cantidadDeAceitunas (Capa (Aceitunas 8) 
--                           (Capa Queso Prepizza)) 
--   ​=​ cantidadDeAceitunas 
--       (conAceitunasJuntas  
--          (Capa (Aceitunas 8) 
--                (Capa Queso Prepizza))) 
{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
        aplicamos Def. cantidadDeAceitunas. x<- Capa (Aceitunas 8) (Capa Queso Prepizza)

    8
        obtenemos como resultado "8"

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeAceitunas (conAceitunasJuntas  (Capa (Aceitunas 8) (Capa Queso Prepizza))) 
        aplicamos Def. conAceitunasJuntas. x<- Capa (Aceitunas 8) (Capa Queso Prepizza) 

    cantidadDeAceitunas ( Capa (Aceitunas 8) (Capa Queso Prepizza)) 
        aplicamos Def. cantidadDeAceitunas. x<- Capa (Aceitunas 8) (Capa Queso Prepizza)

    8
        obtenemos como resultado "8"

    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!
    
--}

-- d. cantidadDeAceitunas (Capa (Aceitunas 9)  (Capa (Aceitunas 8) 
--                                 (Capa Queso Prepizza))) 
--   ​=​ cantidadDeAceitunas 
--       (conAceitunasJuntas  
--          (Capa (Aceitunas 9)  
--                (Capa (Aceitunas 8) 
--                      (Capa Queso Prepizza)))) 

{-- 
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeAceitunas (Capa (Aceitunas 9)  (Capa (Aceitunas 8) (Capa Queso Prepizza))) 
        aplicamos Def. cantidadDeAceitunas. x<- Capa (Aceitunas 9)  (Capa (Aceitunas 8) (Capa Queso Prepizza))

    17
        obtenemos como resultado "17"

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeAceitunas (conAceitunasJuntas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))) 
       aplicamos Def. conAceitunasJuntas. x<- Capa (Aceitunas 9)  (Capa (Aceitunas 8) (Capa Queso Prepizza))

    cantidadDeAceitunas ( Capa (Aceitunas 17) (Capa Queso Prepizza)  )
        aplicamos Def. cantidadDeAceitunas. x<- Capa (Aceitunas 17) (Capa Queso Prepizza) 

    17
        obtenemos como resultado "17"
    
    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!
--}


-- CONSEJO: al escribirlo en papel, usar abreviaturas (e.g. ​cantAcs por                   
-- cantidadDeAceitunas​, ​conDescM por ​conDescripcionMejorada​, ​C por           
-- Capa​, ​Ac​ por ​Aceitunas​,​ Q​ por ​Queso​ y ​Pp​ por ​Prepizza​). 
   


--                                      SECCION 2 



-- a. largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) 
--   ​=​ cantidadDeIntegrantes (Becario "Alan") 
{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    largoDePlanilla (planillaDeIntegrantes (Becario "Alan"))    
        aplico def. planillaDeIntegrantes, x<- (Becario "Alan")

    largoDePlanilla (Registro "Alan" Fin))    
        aplico def. largoDePlanilla, x<- Registro "Alan" Fin

    1
        obtenemos como resultado "1"


LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeIntegrantes (Becario "Alan") 
        aplico def. cantidadDeIntegrantes, x<- (Becario "Alan")

    1
        obtenemos como resultado "1"

    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!

--}


-- b. largoDePlanilla (planillaDeIntegrantes (Becario "Brian")) 
--   ​=​ cantidadDeIntegrantes (Becario "Brian") 
{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    largoDePlanilla (planillaDeIntegrantes (Becario "Brian")) 
        aplico def. planillaDeIntegrantes, x<- (Becario "Brian")

    largoDePlanilla (Registro "Brian" Fin))    
        aplico def. largoDePlanilla, x<- Registro "Brian" Fin

    1
        obtenemos como resultado "1"

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeIntegrantes (Becario "Brian") 
        aplico def. cantidadDeIntegrantes, x<- (Becario "Brian")

    1
        obtenemos como resultado "1"

    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!

--}


-- c. para todo ​n​ :: Nombre ​. 
--   ​largoDePlanilla (planillaDeIntegrantes (Becario ​n​))   
--    ​=​ cantidadDeIntegrantes (Becario ​n​) 
{-- 
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    ​largoDePlanilla (planillaDeIntegrantes (Becario ​n​))  
        aplico def. planillaDeIntegrantes, x <- (Becario ​n​)

    ​largoDePlanilla (Registro ​n Fin)  
        aplico def. planillaDeIntegrantes, x <- (Becario ​n​)

    1
        obtenemos como resultado "1", independientemente de quien sea n 

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    cantidadDeIntegrantes (Becario n) 
        aplico def. cantidadDeIntegrantes, x<- (Becario n)

    1
        obtenemos como resultado 1 , independientemente de quien sea n  

    QUEDA DEMOSTRADO PARA TODO N!!! LLEGAMOS AL MISMO RESULTADO!
--}


-- d. largoDePlanilla 
--    (planillaDeIntegrantes 
--       (Investigador "Alonzo" (Becario "Alan") 
--                              (Becario "Alfred") 
--                              (Becario "Stephen"))) 
--   ​=​ cantidadDeIntegrantes 
--       (Investigador "Alonzo" (Becario "Alan") 
--                              (Becario "Alfred") 
--                              (Becario "Stephen")) 
{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) 
        aplico def. planillaDeIntegrantes, x <- Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")

    largoDePlanilla (Registro "Alonzo" (Regisro "Alan" Fin) (Registro "Alfred" Fin) (Registro "Stephep" Fin))
        aplico def. largoDePlanilla, x <-Registro "Alonzo" (Regisro "Alan" Fin) (Registro "Alfred" Fin) (Registro "Stephep" Fin)

    
    4 
        obtenemos como resultado "4"

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
         aplico def. cantidadDeIntegrantes, x <- Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")

    4 
        obtenemos como resultado "4"
    
    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!
--}


-- e. para todo ​n​ :: Nombre ​. para todo ​n1​ :: Nombre ​.  
--      para todo ​n​2 :: Nombre ​. para todo ​n3​ :: Nombre ​. 
-- largoDePlanilla 
--    (planillaDeIntegrantes 
--       (Investigador ​n​ (Becario ​n1​) 
--                       (Becario ​n2​) 
--                       (Becario ​n3​))) 
--   ​=​ cantidadDeIntegrantes 
--       (Investigador ​n​ (Becario ​n1​) 
--                       (Becario ​n2​) 
--                       (Becario ​n​3)) 
{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    largoDePlanilla (planillaDeIntegrantes (Investigador ​n​ (Becario ​n1​) (Becario ​n2​) (Becario ​n3​))) 
        aplico def. planillaDeIntegrantes, x <- Investigador n (Becario n1) (Becario n2) (Becario n3)

    largoDePlanilla (Registro n (Regisro n1 Fin) (Registro n2 Fin) (Registro n3 Fin))
        aplico def. largoDePlanilla, x <- Registro n (Regisro n1 Fin) (Registro n2 Fin) (Registro n3 Fin)

    4 
    
        obtenemos como resultado 4 , independientemente de quien sea n  

LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeIntegrantes (Investigador ​n​ (Becario ​n1​) (Becario ​n2​)  (Becario ​n​3)) 
        aplico def. cantidadDeIntegrantes, x <-Investigador ​n​ (Becario ​n1​) (Becario ​n2​)  (Becario ​n​3)

    4 
        obtenemos como resultado 4 , independientemente de quien sea n  

    QUEDA DEMOSTRADO PARA TODO N!!! LLEGAMOS AL MISMO RESULTADO!
--}


-- f. largoDePlanilla 
--    (planillaDeIntegrantes 
--       (Investigador "Oswald" 
--          (Investigador "Alonzo" (Becario "Alan") 
--                                 (Becario "Alfred") 
--                                 (Becario "Stephen")) 
--          (Investigador "John"   (Becario "Brian") 
--                                 (Becario "Graham") 
--                                 (Becario "Ioan")) 
--          (Investigador "Robert" (Becario "Gordon") 
--                                 (Becario "John") 
--                                 (Becario "Raymond")))) 
--   ​=​ cantidadDeIntegrantes 
--       (Investigador "Oswald" 
--          (Investigador "Alonzo" (Becario "Alan") 
--                                 (Becario "Alfred") 
--                                 (Becario "Stephen")) 
--          (Investigador "John"   (Becario "Brian") 
--                                 (Becario "Graham") 
--                                 (Becario "Ioan")) 
--          (Investigador "Robert" (Becario "Gordon") 
--                                 (Becario "John") 
--                                 (Becario "Raymond"))))
{--
LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
largoDePlanilla 
    (planillaDeIntegrantes 
       (Investigador "Oswald" 
          (Investigador "Alonzo" (Becario "Alan") 
                                 (Becario "Alfred") 
                                 (Becario "Stephen")) 
          (Investigador "John"   (Becario "Brian") 
                                 (Becario "Graham") 
                                 (Becario "Ioan")) 
          (Investigador "Robert" (Becario "Gordon") 
                                 (Becario "John") 
                                 (Becario "Raymond")))) 

    aplico def. planillaDeIntegrantes, x <- Todo lo anterior 

largoDePlanilla
    (Registro "Oswald" 
          (Registro "Alonzo" (Registro "Alan" Fin) 
                                 (Registro "Alfred" Fin) 
                                 (Registro "Stephen" Fin)) 
          (Registro "John"   (Registro "Brian" Fin) 
                                 (Registro "Graham" Fin) 
                                 (Registro "Ioan" Fin)) 
          (Registro "Robert" (Registro "Gordon" Fin) 
                                 (Registro "John" Fin) 
                                 (Registro "Raymond" Fin)))) 

    aplico def. largoDePlanilla, x <- Todo lo anterior 

    13  

    Obtenemos como resultado "13"
      

LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !

    cantidadDeIntegrantes 
       (Investigador "Oswald" 
          (Investigador "Alonzo" (Becario "Alan") 
                                 (Becario "Alfred") 
                                 (Becario "Stephen")) 
          (Investigador "John"   (Becario "Brian") 
                                 (Becario "Graham") 
                                 (Becario "Ioan")) 
          (Investigador "Robert" (Becario "Gordon") 
                                 (Becario "John") 
                                 (Becario "Raymond"))))

        aplico def. cantidadDeIntegrantes, x <- Todo lo anterior

    13  

        Obtenemos como resultado "13" 

    DEMOSTRADO!!! LLEGAMOS AL MISMO RESULTADO!

--}



-- Ejercicio 4) Demostrar las siguientes propiedades: 
-- a. para todo ​p​ :: Planilla ​. 
--     ​largoDePlanilla (juntarPlanillas Fin ​p​) 
--    ​=​ largoDePlanilla Fin + largoDePlanilla ​p 
{-- 
LADO DERECHO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !
    ​largoDePlanilla (juntarPlanillas Fin ​p​) 


LADO IZQUIERDO. YA HAY PRINCIPIO DE EXTENCIONABILIDAD !


--}





-- b. para todo ​p​ :: Planilla ​. 
--     ​largoDePlanilla  
--     (juntarPlanillas (Registro "Edsger" Fin) ​p​) 
--    ​=​ largoDePlanilla (Registro "Edsger" Fin) 
--        + largoDePlanilla ​p 
 
-- Página 3 de 7 
-- Programación Funcional 
-- c. para todo ​p​ :: Planilla ​. 
--     ​largoDePlanilla 
--     (juntarPlanillas (Registro "Alan" 
--                         (Registro "Edsger" Fin)) 
--                      ​p​) 
--    ​=​ largoDePlanilla (Registro "Alan" 
--                         (Registro "Edsger" Fin)) 
--        + largoDePlanilla ​p 
-- d. para todo ​p​ :: Planilla ​. 
--     ​largoDePlanilla 
--     (juntarPlanillas (Registro "Alonzo" 
--                         (Registro "Alan" 
--                           (Registro "Edsger" Fin))) 
--                      ​p​) 
--    ​=​ largoDePlanilla (Registro "Alonzo" 
--                         (Registro "Alan" 
--                           (Registro "Edsger" Fin))) 
--        + largoDePlanilla ​p 



