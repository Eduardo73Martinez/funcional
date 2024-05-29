
class Gato:
    def __init__(self,peso, edad, nombre):  
        self.peso = peso 
        self.edad = edad 
        self.nombre = nombre   

    def geterNombre(self): 
        return self.nombre 
    def geterPeso(self) : 
        return self.peso
    def geterEdad(self):
        return self.edad
    
def sumar( a,  b):
    return a + b

michita = Gato(300, 3 ,"michita" )
micha =  Gato(600, 4 ,"micha" )


print(micha.geterNombre())
print(micha.geterEdad())
print(micha.geterPeso())





