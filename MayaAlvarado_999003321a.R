#Examen parical 1
#Maya Luz Alvarado Avila, carné 999003321, Grupo 2, Sección A


table <- (airquality)
View(airquality)
summary(airquality)

#1 Utilizando el conjunto de datos "airquality" ¿Cuál es la media, mediana y moda de la variable Wind?
mean(airquality$Wind)
median(airquality$Wind)
library(modeest)
mfv(airquality$Wind)

#2Utilizando el conjunto de datos "airquality" indique qué desviación estándar es más adecuada. ¿La de Wind o la de Ozono?
sd(airquality$Wind)
sd(airquality$Ozone, na.rm = TRUE)

#3Utilizando el conjunto de datos "airquality" ¿Cuál es la media, mediana y moda de la variable Temp?
mean(airquality$Temp)
median(airquality$Temp)
mfv(airquality$Temp)

4#Utilizando el conjunto de datos "airquality" grafique el histograma de la variable Ozono e indique qué tipo de asimetría tiene
hist(x = airquality$Ozone, main = "Histograma de Ozono", 
     xlab = "Ozono", ylab = "Frecuencia",
     col = "skyblue")

#5Utilizando el conjunto de datos "airquality" 
#calcule el grado de simetría de la variable SolarR. ¿Qué significa el valor de su simetría?
library(psych)
skew(airquality$Solar.R)

#6Utilizando el conjunto de datos "diamonds" y la librería GGPLOT y realizando un 
#diagrama de dispersión entre las variables Carat y Price indique la relación de las variables
?diamonds
library(ggplot2)
ggplot(data = diamonds, aes(x = diamonds$carat, y = diamonds$price)) + 
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_smooth(color = 'purple') + 
  xlab('Carat') + 
  ylab('Price') +
  ggtitle('Relación entre Carat y Price de Diamonds') + 
  theme_minimal()

#7Utilizando el conjunto de datos "diamonds" y la librería GGPLOT,
#diagrame una gráfica de barras con la variable color. ¿Cual es el color con mayor frecuencia?
ggplot(data = diamonds, aes(x =diamonds$color, fill = as.factor(diamonds$color))) + 
  geom_bar() + 
  xlab("Color") + 
  ylab("Frecuencias") + 
  ggtitle("Gráfico de Barras Color Diamonds") +
  labs(fill = "color") +   theme_minimal()

#8Utilizando el conjunto de datos "diamonds" y la librería GGPLOT realice un histograma con la 
#variable Carat en su eje X. ¿Qué tipo de simetría presenta la gráfica?
ggplot(diamonds) +
  geom_histogram(binwidth = 0.2, aes(x = carat), fill = 'orange') + 
                   xlab("Carat") + ylab("Frecuencia") + 
                   ggtitle ("Distribucion quilates") + theme_minimal ()

#9. Utilizando el conjunto de datos "diamonds" y la librería GGPLOT y realizando el diagrama de 
#cajas entre las variables Carat y Price indique cuál de las siguientes opciones es correcta
ggplot(data = diamonds, aes(x = diamonds$carat, y = diamonds$price)) + 
  geom_boxplot(aes(color = carat), alpha = 1) + 
  xlab('quilates') + 
  ylab('precio') +
  ggtitle('quilates por precio') + 
  theme_minimal()

#10 Utilizando el conjunto de datos "diamonds" y la librería GGPLOT y realizando un diagráma de dispersión entre las
#variables "Y" y "Z", indique la relación entre las variables (Seleccione 2 posibles respuestas)
ggplot(data = diamonds, aes(x = diamonds$y, y = diamonds$z)) + 
  geom_point(color = 'slateblue', size = 4, alpha = 1) +
  geom_smooth(color = 'orange') + 
  xlab('y') + 
  ylab('z') +
  ggtitle('Relación entre y y z') + 
  theme_minimal()

#11 Se ha tomado una muestra de 20 elementos, donde la probabilidad de éxito es del 60%. 
#Calcule la probabilidad de densidad y grafique sus valores. ¿Qué tipo de sesgo se observa en los resultados?
n <- 20
p <- 0.60
q <- 0.40
x <- 1:n

prob=dbinom(x,n,p)
barplot(prob, main = "Probabilidad densidad", names.arg =x)

#12Una oficina de servicio al cliente recibe en promedio 20 quejas en un día.
#Calcule la probabilidad de densidad y grafique sus valores. ¿Qué observa en la gráfica?
xpoisson= 1:24
lambda= 20

distpoison <- dpois(xpoisson, lambda)

distpoison
plot (xpoisson, distpoison, type="h", col=c("orange", "yellow", "gray"), xlab = xpoisson)
barplot(distpoison)

