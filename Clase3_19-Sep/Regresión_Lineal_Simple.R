#Clase 3
#Vamos a trabajar con un dataset de la encuesta de sueldos en el sector de tecnología en Argenina realizada por SysArmy.
#El objetivo es crear un modelo lineal simple para explicar el sueldo neto de los Data Analysts, Data Scientists y Data Engineers en Argentina.
rm(list=ls())
# Carga de librerías
library(tidyverse)
library(corrr)
library(knitr)
library(kableExtra)
library(GGally)

#Cargando datos de encuesta
encuesta <- read_csv("../Clase3_19-Sep/encuesta_sueldos_sysarmy_1s2020.csv")
encuesta %>%
  glimpse()

str(encuesta)

#Filtramos los perfiles de Data Analysts, Data Scientists y Data Engineers
encuesta_ds = encuesta %>%
  filter(trabajo_de %in% c("Data Scientist / Data Engineer", "BI Analyst / Data Analyst"))
# Función para describir las dimensiones
dim_desc(encuesta_ds)
str(encuesta_ds)


#Análisis explotatorios, valores únicos y faltantes
tabla_exploratorios =  encuesta_ds %>%
  gather(., 
         key = "variables", 
         value = "valores") %>% # agrupamos por las variables del set
  group_by(variables) %>% 
  summarise(valores_unicos = n_distinct(valores),
            porcentaje_faltantes = sum(is.na(valores))/nrow(encuesta_ds)*100) %>% 
  arrange(desc(porcentaje_faltantes), valores_unicos) # ordenamos por porcentaje de faltantes y valores unicos

tabla_exploratorios

#Valdría la pena hacer un análisis de texto de la siguietne variable
table(encuesta$cuales_consideras_que_son_las_mejores_empresas_de_it_para_trabajar_en_este_momentoen_tu_ciudad)
table(encuesta_ds$cuales_consideras_que_son_las_mejores_empresas_de_it_para_trabajar_en_este_momentoen_tu_ciudad)

#Visualización gráfica de variables con faltantes
tabla_exploratorios %>% filter(porcentaje_faltantes>0) %>% 
  ggplot(., aes(x=reorder(variables, -porcentaje_faltantes), y=porcentaje_faltantes, fill=porcentaje_faltantes)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(high = "firebrick", low = "orange") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 18)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=80, vjust=0.5), legend.position = "none") +
  labs(title='Porcentaje de valores faltantes', y='Porcentaje de faltantes', x='') 


#Seleccionamos las variables que nos interesan. En este caso incluiremos la variable a predecir salario neto y 7 variables más.
encuesta_ds_relevantes = encuesta_ds %>%
  # Seleccionamos las variables de interés
  select(tengo, anos_de_experiencia, anos_en_la_empresa_actual, anos_en_el_puesto_actual,
         trabajo_de, salario_mensual_bruto_en_tu_moneda_local,
         salario_mensual_neto_en_tu_moneda_local, sueldo_dolarizado) %>% 
  #Renombramos algunas variables
  rename(edad = tengo,
         perfil = trabajo_de,
         salario_bruto = salario_mensual_bruto_en_tu_moneda_local,
         salario_neto = salario_mensual_neto_en_tu_moneda_local)


#Realizamos un primer análisis descriptivo de nuestro dataset de columnas relevantes. Para ello utilizamos la función 
#ggpairs sobre las variables numéricas con una apertura por la variable de perfil.

encuesta_ds_relevantes %>% select(-sueldo_dolarizado) %>%
  ggpairs(aes(color=perfil), upper = list(continuous = wrap("cor", size = 3, hjust=0.8, alignPercent=0.15)), legend = 25) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")

#Delimitando por sueldos NO dolarizados
encuesta_ds_relevantes %>% 
  filter(sueldo_dolarizado==1) %>% # Observaciones con el sueldo dolarizado
  nrow()

#Eliminamos los faltantes de sueldo neto (son 10)
encuesta_ds_relevantes = encuesta_ds_relevantes %>% 
  drop_na(salario_neto)

#Análisis de datos inconsistentes edad vs. años de experiencia
ggplot(encuesta_ds_relevantes, aes(x = edad, y = anos_de_experiencia)) +
  geom_point() +
  theme_bw() +
  labs(x="Edad", y="Años de experiencia", title = "Edad vs Años de experiencia")

#Se ven casos donde la experiencia supera la edad o donde se tienen por ej 20 años de edad con 16 de experiencia
edad_laboral_minima = 14
encuesta_ds_relevantes %>% 
  filter(edad-anos_de_experiencia<=edad_laboral_minima) %>% # filtro de los problemas con la edad
  select(edad, anos_de_experiencia)
#9 casos donde necesitamos filtrar dado que se tiene la problematica mencionada

#Outlier años en la empesa
ggplot(encuesta_ds_relevantes, aes(y=anos_en_la_empresa_actual)) + geom_boxplot() + theme_bw()
#Se filtra el outlier de 2000
encuesta_ds_relevantes %>% filter(anos_en_la_empresa_actual>70)

#Salario neto superior al bruto
ggplot(encuesta_ds_relevantes, aes(x=salario_bruto, y=salario_neto, color = salario_neto<salario_bruto)) +
  geom_point() +
  theme_bw() +
  labs(title = "Inconsistencias en el salario", x="Salario Bruto", y="Salario Neto", color="¿Registro consistente?")

encuesta_ds_relevantes %>%
  filter(salario_bruto<salario_neto) %>% 
  select(salario_bruto, salario_neto)

#Filtramos acorde a las problematiacs vistas
encuesta_ds_filtrada = encuesta_ds_relevantes %>%
  filter(sueldo_dolarizado==0, #Eliminamos los sueldos dolarizados
         edad-anos_de_experiencia>=edad_laboral_minima, # Eliminamos registros inconsistentes con la edad laboral
         salario_bruto>salario_neto, # Inconsistencia en los sueldos
         anos_en_la_empresa_actual<70) %>%  # Error de carga
  select(-sueldo_dolarizado) # Eliminamos la columna de sueldo dolarizado

#Graficamos de nuevo tras los filtros
encuesta_ds_filtrada %>%
  ggpairs(aes(color=perfil), upper = list(continuous = wrap("cor", size = 3, hjust=0.8, alignPercent=0.15)), legend = 25) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")

#Se siguen viendo correlaciones muy débiles
#Se aprecia en los plots que se siguen teniendo outliers

#Outliers en salarios netos
ggplot(encuesta_ds_filtrada, aes(y=salario_neto)) +
  geom_boxplot() +
  theme_bw() +
  labs(title="Distribución salario neto", y="Salario Neto")

criterio_intercuantil = 1.5 #1.5 por defecto 
limite_superior_outliers = IQR(encuesta_ds_filtrada$salario_neto) * criterio_intercuantil + quantile(encuesta_ds_filtrada$salario_neto, 0.75)[[1]]
limite_superior_outliers

#Cuantos registros superan este límite
encuesta_ds_filtrada %>% filter(salario_neto>limite_superior_outliers)

limite_inferior_outliers =quantile(encuesta_ds_filtrada$salario_neto, 0.25)[[1]] - IQR(encuesta_ds_filtrada$salario_neto) * criterio_intercuantil
limite_inferior_outliers 
#El límite inferior es negativo, filtrmos sólo por el superior
encuesta_ds_sin_outliers = encuesta_ds_filtrada %>% filter(salario_neto<=limite_superior_outliers)

#Análisis por percentiles, cola izquierada distribución
# Funcion para crear una tabla con los percentiles deseados de la variable
crear_tabla_percentiles <- function(vector, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ vector }}" := quantile(vector, q), "{{ vector }}_q" := q)
}

# Creamos el vector del salario
salario_neto_vec = encuesta_ds_sin_outliers$salario_neto

# Creamos la tabla de percentiles en intervalos de a 5%
percentiles_salario_neto = crear_tabla_percentiles(salario_neto_vec, c(seq(0,.1,0.01)))
percentiles_salario_neto

# Seleccionamos el valor del salario neto del percentil 5
limite_inferior_percentil = percentiles_salario_neto %>% 
  filter(salario_neto_vec_q==0.05) %>%
  select(salario_neto_vec) %>%
  as.numeric()

#Eliminamos los outliers inferiores según este criterio. Al tratarse del percentil 5, sabemos que estamos perdiendo 
#5% de los datos.
encuesta_ds_final = encuesta_ds_sin_outliers %>%
  filter(salario_neto >= limite_inferior_percentil)


#Graficamos de nuevo las variables para la encuesta final
encuesta_ds_final %>%
  ggpairs(aes(color=perfil),upper = list(continuous = wrap("cor", size = 3, hjust=0.8, alignPercent=0.15)), legend = 25) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")

# calculo matriz de correlacion para los registros completos (omitiendo faltantes) para variables numéricas con ambos métodos 
# pearson
matriz.correl.pe <- encuesta_ds_final %>%
  select_if(is.numeric) %>% # Seleccionamos las variables numéricas
  correlate(use = "complete.obs", method = "pearson") %>% 
  shave() %>% 
  fashion()
matriz.correl.pe

# Basic box plot
p <- ggplot(encuesta_ds_final, aes(x=salario_neto, y=perfil)) + 
  geom_boxplot()
p
# Rotate the box plot
#options(scipen=0) #PAra deshabilitar notación científica

# spearman
matriz.correl.sp <- encuesta_ds_final %>%
  select_if(is.numeric) %>% # Seleccionamos las variables numéricas
  correlate(use = "complete.obs", method = "spearman") 
matriz.correl.sp

#Modelo EDAD
#salarioNeto=β0+β1Edad+ϵ
# Crear el modelo lineal de la edad
modelo_edad = lm(formula = salario_neto ~ edad, data = encuesta_ds_final)
# Observamos que devuelve el modelo
modelo_edad 
modelo_edad$coefficients

#Teóricamente el valor β0^=20,223.59 es el salario neto esperado para una persona recién nacida (0 años). 
#Por este motivo este valor carece de interpretación práctica.
#El valor de β1^ indica que por cada aumento de un año de edad el salario neto esperado aumenta en $1,364.72

# Accedemos a la información de los coeficientes estimados
intercepto = modelo_edad$coefficients[1]
pendiente = modelo_edad$coefficients[2]

# Graficamos el dataset y el modelo
encuesta_ds_final %>% ggplot(., aes(x = edad, y = salario_neto)) + 
  geom_abline(intercept = intercepto, slope = pendiente, color="forestgreen", size=1.5) + # capa del modelo
  geom_point() + #capa de los datos
  theme_bw() +
  scale_x_continuous(limits = c(0,60)) +
  scale_y_continuous(limits = c(0,150000)) +
  labs(title="Modelo Lineal Simple: Edad", x="Edad", y="Salario Neto") 

#Modelo Años experiencia
#SalarioNeto=β0+β1AñosExperiencia+ϵ
# Crear el modelo lineal de los años de experiencia
modelo_experiencia = lm(formula = salario_neto ~ anos_de_experiencia, data = encuesta_ds_final)
modelo_experiencia$coefficients
#El valor de β0^ indica que el salario neto esperado es de $51,825.26 para una persona con perfil de Data 
#Scientist/Engineer o Data Analyst sin experiencia laboral. (Argentina 2019)
#El valor de β1^ indica que por cada aumento de un año de experiencia el salario neto esperado aumenta en $1,693.57.
# Accedemos a la información de los coeficientes estimados
intercepto = modelo_experiencia$coefficients[1]
pendiente = modelo_experiencia$coefficients[2]

# Graficamos el dataset y el modelo
encuesta_ds_final %>% ggplot(., aes(x = anos_de_experiencia, y = salario_neto)) + 
  geom_abline(intercept = intercepto, slope = pendiente, color="steelblue", size=1.5) + # capa del modelo
  geom_point() + #capa de los datos
  theme_bw() +
  scale_x_continuous(limits = c(0,40)) +
  scale_y_continuous(limits = c(0,150000)) +
  labs(title="Modelo Lineal Simple: Años de experiencia", x="Años de experiencia", y="Salario Neto") 

#Modelo años en la empresa actual
#salarioNeto=β0+β1AñosEmpresa+ϵ
# Crear el modelo lineal de los años de experiencia
modelo_empresa = lm(formula = salario_neto ~ anos_en_la_empresa_actual, data = encuesta_ds_final)
modelo_empresa$coefficients
#bEl valor de β0^ indica que el salario neto esperado es de $60,301.46 para una persona con perfil de
#Data Scientist/Engineer o Data Analyst que recién comienza en una empresa.
#El valor de β1^ indica que por cada aumento de un año de experiencia en la misma empresa el salario 
#neto esperado aumenta en $618.58.

# Accedemos a la información de los coeficientes estimados
intercepto = modelo_empresa$coefficients[1]
pendiente = modelo_empresa$coefficients[2]

# Graficamos el dataset y el modelo
encuesta_ds_final %>% ggplot(., aes(x = anos_en_la_empresa_actual, y = salario_neto)) + 
  geom_abline(intercept = intercepto, slope = pendiente, color="firebrick", size=1.5) + # capa del modelo
  geom_point() + #capa de los datos
  theme_bw() +
  scale_x_continuous(limits = c(0,40)) +
  scale_y_continuous(limits = c(0,150000)) +
  labs(title="Modelo Lineal Simple: Años en la empresa ", x="Años en la empresa actual", y="Salario Neto") 



write.csv(encuesta_ds_final,"../Clase4_26-Sep/encuesta_sueldos_sysarmy_limpia.csv", row.names = TRUE)




