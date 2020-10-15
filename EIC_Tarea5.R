#Integrantes
#Angélica Carolina Rodríguez Ruiz	-	999003306
#María Fernanda Monterroso Colindres  - 999003305
#Maya Luz Alvarado Avila - 999003321

###########EJERCICIO 1 
#Ho: kilómetros recorridos ≥ 20000
#Hi: kilómetros recorridos < 20000

alfa1 <- 0.03
n1 <- 100
media1 <- 19500
mu1 <- 20000
sd1 <- 3900

#5 Obtener valor del estadístico de prueba
z1<- (media1-mu1)/(sd1/sqrt(n1))
z1
#R/ -1.28

#6 Obtener valor de la distribución
zAlfa1<- qnorm(alfa1,0,1, lower.tail = TRUE)
zAlfa1
#R/ -1.88

#7 Validar la hipótesis
z1 <  zAlfa1
#R/ FALSE



##########Ejercicio 2
#H0: varianza ≤ 40
#Hi: varianza > 40

alfa2 <- 0.05
n2 <- 10
s2 <- 27
sigma <- 40

#5 Obtener valor del estadístico de prueba
X2 <- ((n2-1)*s2)/sigma
X2
#R/6.075

#6 Obtener valor de la distribución
chi_sd2<- qchisq(1-alfa2, n2-1)
chi_sd2
#R/16.91

#7 Validar la hipótesis
X2 > chi_sd2
#R/ FALSE, se acepta H0



#########Ejercicio 3
#1 Definir variables
n3 <- 30
media3 <- 35
sd3 <- 3.5
alfa3 <- 0.05/2

#2 Defina normal ajustada
normal1 <- qnorm(1-alfa3, 0, 1)
normal1
#R/ 1.96

#3 Defina Error
error1<-  sd3/sqrt(n3)
error1
#R/ 0.639

#4 Defina limites superior e inferior del intervalo de confianza 
margen <- normal1*error1
margen

limInf<- media3 - margen
limsup <- media3 + margen
limInf
limsup
#R/límite superior 36.25, límite inferior 33.74



#######Ejercicio 4
#1 Definir variables
n4 <- 5000
media4 <- 500
sd4 <- 100
alfa4 <- 0.10/2

#2 Defina normal ajustada
normal2 <- qnorm(1-alfa4, 0, 1)
normal2
#R/1.64

#3 Defina Error
error2<-  sd4/sqrt(n4)
error2
#R/1.41

#4 Defina limites superior e inferior del intervalo de confianza
margen1 <- normal2*error2
margen1

limInf0<- media4 - margen1
limsup0 <- media4 + margen1
limInf0
limsup0
#R/límite superior 502.33, límite inferior 497.67



#########Ejercicio 5
data("trees")

#1 Definir regresión entre Girth (y) y Height(x)
lm.trees <- lm(Girth~Height, data=trees)
summary(lm.trees)
plot (trees$Girth ~trees$Height)
abline (lm.trees)

#2 Indique la formula generada

#3 Indique su intercepto, coeficiente de correlación R2 y R ajustado
#R/ intercepto -6.18, coeficiente de correlacion 26.97%, R ajustado 24.45%

#4 Definir regresión entre Girth  y volumen (x)
lm.trees2 <- lm(Girth~Volume, data=trees)
summary(lm.trees2)
plot (trees$Girth ~trees$Volume)
abline (lm.trees2)

#5 Indique la formula generada

#6 Indique su intercepto, coeficiente de correlación R2 y R ajustado
#R/ intercepto 7.67, coeficiente de correlacion 93.53%, R ajustado 93.31%
