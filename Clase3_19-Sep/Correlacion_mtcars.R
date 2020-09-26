#Clase EEA 3 Correlación

library(tidyverse)
library(openintro)
library(GGally)
library(corrr)
library(knitr)
library(kableExtra)
library(mvnormtest)

options(knitr.table.format = "html") 

#Preview Dataset
glimpse(mtcars)
str(mtcars)
#Preview en modo tabla mejor presentado
mtcars %>% 
  head() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped"))

#Análisis entre variables
#mpg: Miles/(US)gallon. Eficiencia de combustible (millas por galón)
#hp : Gross horsepower. Potencia del motor
ggplot(mtcars, aes(x = hp, y = mpg)) + 
  geom_point()

#Se valida la distribución de las variables, parecieran tener una correlación lineal
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, aes(y = ..density..), position = "identity", alpha = 0.5, fill = "#1B9E77", color = "white")+
  geom_density(alpha = 0.6)+
  theme(legend.position="top") + 
  theme_classic()

ggplot(mtcars, aes(x = hp)) +
  geom_histogram(bins = 10, aes(y = ..density..), position = "identity", alpha = 0.5, fill = "#1B9E77", color = "white")+
  geom_density(alpha = 0.6)+
  theme(legend.position="top") + 
  theme_classic()

# calculamos covarianza 
cov(mtcars$mpg, mtcars$hp)

# calculamos correlación
cor(mtcars$mpg, mtcars$hp) # pearson por default

#Inferencia de ρ
#¿Qué podemos decir de ρ a partir de r?
#Queremos sacar conclusiones acerca del parámetro poblacional ρ a partir de la muestra. 

# aplicamos test
cor.test(mtcars$mpg, mtcars$hp, method = "pearson")

#Coeficiente de Correlación de Spearman
#Medida más robusta que Pearson a outliers, no necesita el supuesto de normalidad para testearse.
cor.test(mtcars$mpg, mtcars$hp, method = "spearman")

#Con los p-valor resultanes de ambos test, se rechazaría la H0 (no existe asociación lineal entre las variables),
#es decir, existe evidencia estadísticamente significativa a favor de la asociación lineal entre las variables mpg y hp.

#Testeando normalidad multivariada:
mvnormtest::mshapiro.test(t(mtcars[,c("mpg", "hp")])) # chequeo normalidad bivariada

#No se cumple normalidad bivariada. Entonces, el test para la correlación de pearson no sería válido ya que 
#no se cumple el supuesto de normalidad multivariada. En cambio, el de spearman no requiere dicho supuesto y 
#sus conclusiones sí serían válidas. Otra alternativa sería buscar normalizar el dataset.

#Graficando la correlación con GGally
#En este caso, vamos a agrupar (colorear) por:
# -am: Tipo de transmisión: automática (am = 0) o manual (am = 1)
mtcars %>% 
  select(-carb,-vs) %>% # desestimamos algunas variables
  mutate(cyl = factor(cyl), 
         am = factor(am)) %>% 
  ggpairs(., 
          title = "Matriz de correlaciones",
          mapping = aes(colour = am))

#Corrr devuelve la matriz de correlación en forma de dataframe
mtcars %>% 
  correlate() %>% # convierte la matriz de corr en dataframe
  shave() %>% # solo muestra información debajo de la diagonal principal
  fashion() # acomoda los datos en forma tidy (por ej. redondeo de decimales)

#Network plot meustra las variables mas relacionadas cerca y por caminos más fuertes
mtcars %>% 
  correlate() %>% 
  network_plot(min_cor = 0.7)

#Rplot para ver visualmente correlación entre variales 
mtcars %>% 
  correlate() %>% 
  rplot()

#¿Y si queremos comparar la relación entre drat y gear?
#drat: la relación de engranaje del eje trasero. En los vehículos con tracción trasera, la relación de eje 
#trasero es una parte importante de una ecuación de remolque exitosa. Se expresa como la relación entre las 
#revoluciones por minuto del eje de transmisión y las revoluciones por minuto del eje trasero. Una relación
#“alta”, con un número alto de rotaciones del eje de transmisión, es mejor para una aceleración rápida, los
#grados de ascenso, el transporte de cargas o el remolque. Sin embargo, ofrece menos ahorro de combustible y se
#produce más ruido cuando el vehículo circula a alta velocidad.
#gear: Número de velocidades hacia adelante.
ggplot(mtcars, aes(x = drat, y = gear)) + 
  geom_point()

#No parece haber una relación lineal. Pero la relación entre estas dos variables podría ser diferente entre los 
#automáticos y con transmisión manual. Sabiendo esto, volvamos a calcular los estimadores puntuales de cada grupo.
mtcars %>% 
  group_by(am) %>% 
  summarise(cor = cor(drat, gear))
#Los autos atomáticos parecen tener correlación positiva y alta, mientras los manuales negativa y baja.
#Tomando el primer grupo, graficamos boxplot paralelos de la variable gear para ver cómo se distribuye drat:

mtcars2 <- mtcars %>% filter(am == 0)
ggplot(mtcars2, aes(gear, drat, group = gear, fill = factor(gear)))+
  geom_boxplot(alpha = 0.75)

#No parece muy correcto hacer un test de correlación de pearson, es decir buscar una relación lineal, con una variable
#que sólo toma dos valores (3 y 4).
#Usemos el test de correlación de Spearman, que no necesita cumplir supuesto de normalidad para testearse.

cor.test(mtcars2$gear, mtcars2$drat, method = "pearson")
cor.test(mtcars2$gear, mtcars2$drat, method = "spearman")

#Testeando normalidad multivariada
mvnormtest::mshapiro.test(t(mtcars2[,c("gear", "drat")])) # chequeo normalidad bivariada




