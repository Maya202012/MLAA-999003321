#Maya Luz Alvarado Avila 
#Carné 999003321
#Sección A

#####################Pregunta 11
#Se desea estimar con un nivel de confianza del 95 % la talla media de los hombres de 18 
#o más años de un país. Suponiendo que la desviación típica de las tallas en la población vale 4,
#obtenga el intervalo de confianza con una muestra de n=15 hombres seleccionados al azar, con una media de 173.47
#¿Cuál es el intervalo de confianza con alfa=5%? 
n11<-15
media11<-173.47
sd11<-4
alfa11<-0.05/2

normal11 <- qnorm(1-alfa11, 0, 1)
normal11

error11<-  sd11/sqrt(n11)
error11

margen11 <- normal11*error11
margen11

limInf11<- media11 - margen11
limsup11 <- media11 + margen11
limInf11
limsup11
#R/ Lim inf 171.45, lim sup 175.49


####################Pregunta 12
#¿Cuál es el valor de la normal utilizada con alfa=5%?
n11<-15
media11<-173.47
sd11<-4
alfa11<-0.05/2

normal11 <- qnorm(1-alfa11, 0, 1)
normal11
#R/ normal es igual a 1.96


#####################Pregunta 13
#Si el nivel de significancia fuera 20%, ¿Cuál sería el valor de la normal? 
n11<-15
media11<-173.47
sd11<-4
alfa13<-0.20/2

normal13 <- qnorm(1-alfa13, 0, 1)
normal13
#R/ normal es igual a 1.28


#####################Pregunta 14
#¿Cuál sería el intervalo de confianza con un alfa = 80%?
n11<-15
media11<-173.47
sd11<-4
alfa14<-0.80/2

normal14 <- qnorm(1-alfa14, 0, 1)
normal14

error11<-  sd11/sqrt(n11)
error11

margen14 <- normal14*error11
margen14

limInf14<- media11 - margen14
limsup14 <- media11 + margen14
limInf14
limsup14
#R/ Lim inf 173.21, lim sup 173.73-------Respuesta correcta por ingeniera 172.15-174-79


####################Pregunta 15
n11<-15
media11<-173.47
sd11<-4
alfa11<-0.05/2

normal11 <- qnorm(1-alfa11, 0, 1)
normal11

error11<-  sd11/sqrt(n11)
error11

margen11 <- normal11*error11
margen11
#R/Margen del intervalo con un IC del 5% es 2.02


n11<-15
media11<-173.47
sd11<-4
alfa15a<-0.15/2

normal15a <- qnorm(1-alfa15a, 0, 1)
normal15a

error11<-  sd11/sqrt(n11)
error11

margen15a <- normal15a*error11
margen15a
#R/Margen del intervalo con un IC del 15% es 1.48

n11<-15
media11<-173.47
sd11<-4
alfa13<-0.20/2

normal13 <- qnorm(1-alfa13, 0, 1)
normal13

error11<-  sd11/sqrt(n11)
error11

margen13 <- normal13*error11
margen13
#R/Margen del intervalo con un IC del 20% es 1.32


n11<-15
media11<-173.47
sd11<-4
alfa15b<-0.25/2

normal15b <- qnorm(1-alfa15b, 0, 1)
normal13

error11<-  sd11/sqrt(n11)
error11

margen15b <- normal15b*error11
margen15b
#R/Margen del intervalo con un IC del 25% es 1.18

#R final// Márgenes correctos del 5% (2.02) y 20% (1.32)



####################Pregunta 18
#La duración de las bombillas de 100 watt que fabrica una empresa sigue una distribución normal con una desviación
#de 120 horas. Su vida media está garantizada durante un mínimo de 800 horas. Se escoge al azar una muestra de 
#50 bombillas de un lote y, después de comprobarlas, se obtiene una vida media de 750 horas.  
#¿Con un nivel de significancia del 1% se tiene que rechazar la hipótesis que establece que todas 
#las bombillas tienen una vida mayor o igual a 800hrs? (H0: mu>= 800)

#¿Cuál es el estadístico de prueba?
#H0: mu ≥ 800
#Hi: mu < 800

alfa18 <- 0.01
n18 <- 50
media18 <- 750
mu18 <- 800
sd18 <- 120

z18 <- (media18-mu18)/(sd18/sqrt(n18))
z18
#R/ -2.946278


###################Pregunta 19
zAlfa18<- qnorm(alfa18,0,1, lower.tail = TRUE)
zAlfa18

z18 <  zAlfa18
#R/TRUE, se rechaza H0 y se acepta Hi. La media es menor a 800 horas.



##################Pregunta 20
#La St. Louis Metro Bus Company de Estados Unidos, desea dar una imagen de confiabilidad haciendo que sus conductores
#sean puntuales en los horarios de llegada a las paradas. La empresa desea que haya poca variabilidad en dichos 
#tiempos. En términos de la varianza de los tiempos de llegada de las paradas, la empresa desea que la 
#varianza (sigma20) sea de 4 minutos o menos. Esta prueba de hipótesis se realiza con un nivel de significancia 
#de α = 0.05 Asuma que en una muestra aleatoria de 24 llegadas a cierta parada en una intersección en el centro 
#de la ciudad, la varianza muestral encontrada es s2=4.9

#R/H0: s2 ≤ 4
  #Hi: s2 > 4
  

#################Pregunta 21
#¿Cuál es el valor de chi cuadrado?
alfa20 <- 0.05
n20 <- 24
s20 <- 4.9
sigma20 <- 4

chi_sd20<- qchisq(1-alfa20, n20-1)
chi_sd20
#R/ valor de chi cuadrado 35.17246


################Pregunta 22
#¿Cuál es el estadístico de prueba?
X20 <- ((n20-1)*s20)/sigma20
X20
#R/ el estadístico de prueba es 28.175


################Pregunta 24
#¿Se rechaza o se acepta la H0?
X20 > chi_sd20
#R/ Se acepta H0, el valor de la varianza es de 4 minutos o menos.



#################Pregunta 25
#Tomando en consideración el conjunto de datos "trees" ¿cuál es el grado de correlación entre las variables 
#Volume y Height? 
data("trees")
lm.trees25 <- lm(Height~Volume, data=trees)
lm.trees25
summary(lm.trees25)
#R/ el grado de correlación es 0.36


#################Pregunta 26
#Tomando en consideración el conjunto de datos "trees" ¿cuál es el grado de correlación entre las variables 
#Volume y Girth?
lm.trees26 <- lm(Girth~Volume, data=trees)
lm.trees26
summary(lm.trees26)
#R/el grado de correlación es 0.935


#################Pregunta 27
#Tomando en consideración el conjunto de datos "trees" y realizando una regresión lineal entre Volumen y Girth,
#¿cómo se representa el modelo de regresión lineal? 
summary(lm.trees26)
#R/ Girth 7.677857, volume = 0.184632


################Pregunta 28
#Tomando en consideración el conjunto de datos "trees" y realizando una regresión lineal entre Volumen y Girth
#¿Cuál es el valor de R2 Ajustado?
summary(lm.trees26)
#R/ R2 ajustado es 0.9331


#################Pregunta 29
#Tomando en consideración el conjunto de datos "trees" y con base en los cálculos realizados sobre este conjunto
#con las variables Volumen y Girth, se genera una gráfica de regresión. Observando la gráfica, ¿qué puede deducir
#de ella? 
plot (trees$Girth ~trees$Volume)
abline (lm.trees26)
