#Sesión 5 Sept
#Ejercicio 2
#1) Dataset EPH
#El INDEC1 publica las bases de microdatos de la Encuesta Permanente de Hogares (EPH). 
#Para este ejercicio deberán descargar la base “usu_individual_T120.txt” que corresponde a la base Individual del 
#1er trimestre de 2020 y se encuentra disponible en el campus.
install.packages("dplyr")
#a.Levantar y guardar la base en un objeto llamado individualt120. La misma usa como separador “;”.
library("tibble")
csvTimeBefore <- Sys.time()
individualt120 <- read.csv("UBA R 2020/EEA  Fuentes/usu_individual_T120.txt", sep = ";")
csvTimeAfter <- Sys.time()
timeCSV <- csvTimeAfter - csvTimeBefore
timeCSV
?read.csv
#b.Visualizar el contenido del dataset. Hint: Se puede utilizar la funcion glimpse() de la libreria tibble para visualizar
#el contenido de la base, identificando dimensión, tipos de variables y primeros valores de cada una.
glimpse(individualt120)
str(individualt120)
#c.Guardar la base como un archivo de extensión .RDS con la función saveRDS() y volver a levantarla pero desde el nuevo 
#archivo .RDS y asignarla con el nombre BaseRDS ¿tarda más o menos? Tarda un poco más por RDS
saveRDS(individualt120, "UBA R 2020/EEA  Fuentes/usu_individual_T120.RDS")
rdsTimeBefore <- Sys.time()
individualt120 <- read.csv("UBA R 2020/EEA  Fuentes/usu_individual_T120.RDS", sep = ",")
rdsTimeAfter <- Sys.time()
timeRDS <- rdsTimeAfter - rdsTimeBefore
timeCSV
timeRDS
#rm(individualt120)

#2) Función acumulado
#Crear una función acumulado que calcule el valor acumulado (suma) de una variable numérica a designar X en un dataset también a 
#designar df. Es decir, que brinde el valor resultante de acumulado(df, X).
acumulado <- function(df, x, grouped = 0, includeRows = FALSE){
  print(grouped)
  if(grouped == 0){
    result <- sum(df[x])
    names(result)[1]<-paste("Results")
  }
  else{
    result <- aggregate(df[,x], by=list(Grouped=df[,grouped]), FUN=sum)
    #tapply(df[,x], df[,grouped], FUN=sum)
    names(result)[1]<-paste(grouped)
    names(result)[2]<-paste("poblationalFreq")
  }
  if(includeRows == TRUE){
    frequencyPerVariable <- table(individualt120[,grouped])
    frequencyPerVariableDF <- as.data.frame(frequencyPerVariable)
    result$sampleFreq <- frequencyPerVariableDF$Freq
  }
  return(result)
}

#a.Evalúela para la columna “PONDERA” del dataframe individual_T120.
acumulado(individualt120, "PONDERA")
#sum(individualt120$PONDERA)

#b.Utilizar dicha función para calcular el acumulado de PONDERA por Sexo (variable CH04), sabiendo que 1 = varón y 2 = mujer.
acumulado(individualt120, "PONDERA", "CH04")
#aggregate(individualt120$PONDERA, by=list(CH04=individualt120$CH04), FUN=sum)
#aggregate(individualt120["PONDERA"], by=list(CH04=individualt120["CH04"]), FUN=sum)

#c.Modificar la función anterior ( acumulado2) para que devuelva un vector que contenga la frecuencia poblacional 
#(el acumulado calculado previamente) y la muestral (número de filas del dataset).
#d.Utilizar la función modificada para calcular la frecuencias frecuencias muestrales y poblacionales por Sexo.
acumulado(individualt120, "PONDERA", "CH04", TRUE)


