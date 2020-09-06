#Sesión 5 Sept
#Ejercicio 1
#1) Dataset state.x77
#Crear un dataframe con el dataset de R: state.x77 y contestar las siguientes preguntas:

rawUS <- state.x77
rawUS

class(rawUS)
str(rawUS)
class(rawUS[,1])

rawUS[1,]
sum(rawUS[,1])

tbl2 <- unlist(rawUS)
attributes(tbl2) <- attributes(rawUS)
dataframeUS <- as.data.frame(tbl2)
dataframeUS
class(dataframeUS)
str(dataframeUS)
#a.¿Cuál es la población total de Estados Unidos?
totalPopulation <- sum(dataframeUS$Population)
totalPopulation
#b.¿Cuál es la media de la expectativa de vida?
meanLiveExpectation <- mean(dataframeUS$`Life Exp`)
meanLiveExpectation
#c¿Cual es la mediana del ingreso en pesos argentinos?
medianIncomeUSD <- median(dataframeUS$Income)
medianIncomeUSD
#With 1 USD = 74.44 ARS
medianIncomeARS <- medianIncomeUSD*74.44
medianIncomeARS

#2) Nueva variable
#a.Crear un dataset df_indice solo con las columnas de porcentaje de analfabetismo sobre población (Illiteracy) y tasa de homicidios por 100.000 habitantes (Murder).
df_indice <- (dataframeUS[c("Illiteracy", "Murder")])
df_indice
#b.Crear una nueva variable ilit_murd que contenga la suma de los valores de ambas variables del dataset.
df_indice$IlliteracyPlusMurder <- dataframeUS$Illiteracy + dataframeUS$Murder
df_indice
#c.Encontrar los valores máximos y mínimos de esta nueva variable.
max(df_indice$IlliteracyPlusMurder)
min(df_indice$IlliteracyPlusMurder)


#3) Objetos
#a.Crear un VALOR llamado OBJETO definido como el resultado de la suma: 5+6
objeto <- 5+6
#b.Crear un VECTOR VEC0 que contenga una muestra aleatoria de números del 1 al 10.
VEC0 <- runif(5, 1, 10)
VEC0
#c.Crear 3 vectores ( VEC1, VEC2, VEC3) que sean transformaciones del anterior consistentes en multiplicar por 2, elevar al cuadrado y restarle 2, respectivamente.
VEC1 <- VEC0 * 2
VEC2 <- VEC0 ^ 2
VEC3 <- VEC0 -2
VEC1
VEC2
VEC3
#d.Crear 3 vectores con la misma cantidad de elementos que VEC0, pero con variables string (texto) ( VEC4, VEC5, VEC6). Hint: usar la funcion rep() en los casos que se quiera repetir un caracter.
tamaño <- length(VEC0)
VEC4 <- rep("Vamos", tamaño)
VEC5 <- rep("mi", tamaño)
VEC6 <- rep("Verde", tamaño)
VEC4
VEC5
VEC6
#e.Crear un dataframe DFRAME como combinación de todos los vectores creados previamente.
DFRAME <- data.frame(VEC0, VEC1, VEC2, VEC3, VEC4, VEC5, VEC6)
DFRAME
#f.Crear una lista LA_LISTA con el OBJETO creado en el punto a), alguno de los vectores y el DFRAME del punto e).
LA_LISTA <- list(objeto, DFRAME)
LA_LISTA

#4) Loops
#a.Para todos los valores del vector VEC0, imprimir mediante un loop el triple de dichos valores.
for(i in VEC0){
  triple <- i*3
  print(triple)
}
#b.Armar un loop que itere sobre los valores únicos de la variable VEC6 del dataframe DFRAME e imprima un texto que combine el valor de VEC6 y de VEC0.
for(i in unique(DFRAME$VEC6)){
  print(paste(VEC6,VEC0))
}
#c.Reescribir el VEC1 del DATAFRAME para que sus elementos sean: el doble de VEC_0 cuando éste sea mayor a 2 e iguales a VEC_0 para el resto de los casos. Hint: usar la función ifelse()
VEC0
VEC1
for(i in 1:length(VEC0)){
  if(VEC0[i] > 2){
    VEC1[i] <- VEC0[i]*2
    print("im on if")
  }
  else{
    VEC1[i] <- VEC0[i]
    print("im on else")
  }
}
VEC0
VEC1

#5) Funciones
#a.Crear una función llamada Hola_Mundo que imprima el texto “Hola mundo”
funcion_Hola_Mundo <- function() {
  print("Hola Mundo")
}
funcion_Hola_Mundo()
#b.Crear una función Sumatoria_enteros que devuelva la sumatoria de los números enteros comprendidos entre 1 y un parámetro x a definir. Es decir, una función Sumatoria_enteros(x) que devuelva la sumatoria de los enteros de 1 a x.
sumatoria_enteros <- function(enteroX){
  print(sum(1:enteroX))
}
sumatoria_enteros(5)
#c.Crear una función primer_elem_matriz cuyo parámetro/input X sea una matrix y que devuelva la dimensión de la matriz en cuestión y un texto que diga “El primer elemento es par” en caso de que así lo fuera o “El primer elemento no es par” en caso contrario. Evaluar la función creada para el dataset DFRAME.
primer_elem_matriz <- function(inputX){
  dim(inputX)
  print(paste("La matriz tiene dimensiones de", dim(DFRAME)[1], "filas y", dim(DFRAME)[2], "columnas"))
  oneMatrix <- data.matrix(inputX)
  if(round(oneMatrix[1][1])%%2 == 0){
    print("El primer elemento es par")
  }
  else{
    print("El primer elemento es impar")
  }
}
primer_elem_matriz(DFRAME)

