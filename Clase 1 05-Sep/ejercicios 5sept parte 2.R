#Sesión 5 Sept
#Ejercicio 2
#1) Dataset EPH
#El INDEC1 publica las bases de microdatos de la Encuesta Permanente de Hogares (EPH). 
#Para este ejercicio deberán descargar la base “usu_individual_T120.txt” que corresponde a la base Individual del 
#1er trimestre de 2020 y se encuentra disponible en el campus.

#a.Levantar y guardar la base en un objeto llamado individualt120. La misma usa como separador “;”.
library("tibble")
csvTimeBefore <- Sys.time()
individualt120 <- read.table("UBA R 2020/EEA  Fuentes/usu_individual_T120.txt", sep = ";")
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
individualt120 <- read.table("UBA R 2020/EEA  Fuentes/usu_individual_T120.RDS", sep = ";")
rdsTimeAfter <- Sys.time()
timeRDS <- rdsTimeAfter - rdsTimeBefore
timeCSV
timeRDS
#rm(individualt120)





