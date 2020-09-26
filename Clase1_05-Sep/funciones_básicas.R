#Función paste

paste("Pega", "estas", 4, "palabras", sep = " ")

A <- 2
B <- 55
C <- 3.2

paste(A, B, C, sep = "**")

#Paste0 pega caracteres sin separador
paste0(A,B,C)

#Listas
1:5

#Función sum con lista del 1 al 5
sum(1:5)

#Función mean
mean(1:5, na.rm = TRUE)

#Factores son listas con valores fijos que son sus levels

sabores <- c("Chocolate", "Caramelo", "Vainilla", "Cereza")
saboresLista <- factor(sabores)
levels(saboresLista)
#Factores ordenados
calificacion <- factor(x = c("1.Muy bueno", "2.Bueno", "3.Aceptable", "4.Malo"), ordered = FALSE)
levels(calificacion)
rm(calificacion)

#Operacioens básicas con lista

C <- c(3,5,6)
D <- C + 1:3 # esto es equivalente a hacer 3+1, 5+2, 6+3 
D

#Vectores y acceso con []

E <- c("Carlos", "Federico", "Pedro")
E
E[2]
borrame <- E[2]

#R implicitamente coerciona la longitud de los vectores. Esto se denomina reciclado de vectores (vector recicling), debido a que el vector de menor longitud se repite, o recicla, hasta igualar la longitud del vector más largo.
# sumamos dos vectores de distinta longitud
G <- 1:10 + 1:2 
G 

#para Borrar un elemento se usa rm
rm(borrame)

#Data Frame
INDICE  <- c( 286.4,    262.1,  248.5,
              285.7,    263.5,  250.2,
              285.1,    264.9,  249.0 )


FECHA  <-  c("Mar-20", "Mar-20", "Mar-20",
             "Abr-20", "Abr-20", "Abr-20",
             "May-20", "May-20", "May-20")

GRUPO  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")

Datos <- data.frame(INDICE, FECHA, GRUPO)
Datos

#Acceso con operador $
Datos$GRUPO
#Acceso por []
Datos[3,2]
#Acceso con operador $ y []
Datos$FECHA[2]

#Filtrar con condición
Datos[Datos$FECHA == "May-20",]

#Lista puede agrupar cualquier tipo de entidad
superlista <- list(A,B,C,D,E,FECHA, DF = Datos, INDICE, GRUPO)
superlista

#FOR Loops
for(i in 1:10){
  print(i^2)
}

for(Valores in 1:10){
  print(Valores^2)
}

#Condicional IF
if( 2+2 == 4){
  print("Menos Mal")
}

if( 2+2 == 148.24){
  print("R, tenemos un problema")
}

#Función ifelse
ifelse(2+2 == 4, yes = "Joya", no = "Error")
?ifelse

#Funciones
suma <- function(valor1, valor2) {
  valor1+valor2
}
suma(5,6)

funcion_prueba <- function(parametro1,parametro2) {
  paste(parametro1, parametro2, sep = " <--> ")
}
funcion_prueba(parametro1 = "A ver", parametro2 = "Qué pasa")

Otra_funcion_prueba <- function(parametro1 ,parametro2 = "String default") {
  paste(parametro1, parametro2, sep = " <--> ")
  
}
Otra_funcion_prueba(parametro1 = "Valor 1")

#Funciones anónimas
(function(x) x*2)(10)









