#CLase 4 EEA
#Regresión Lineal Simple: Evaluación y Diagnóstico


rm(list=ls())
gc()

#El objetivo es crear un modelo lineal simple para explicar el sueldo neto de los Data Analysts, Data Scientists y Data Engineers en Argentina.
#salarioNeto=β0+β1X+ϵ 

# Carga de librerías
library(tidyverse)
library(tidymodels)

# Cargamos el dataset limpio
encuesta_sueldos = read_csv("encuesta_sueldos_sysarmy_limpia.csv")



