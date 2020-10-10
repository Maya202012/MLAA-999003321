#Integrantes
#Angélica Carolina Rodríguez Ruiz		999003306
#María Fernanda Monterroso Colindres   999003305
#Maya Luz Alvarado Avila  999003321

#......Ejercicio 1
#H0: peso  = 500 gramos
#Hi: peso <  500 gramos

alfa1 <- 0.05
n1 <- 100
media1 <- 499.4
sd1 <- 5
mu1 <- 500

#5 Obtener el valor del estadístico de preuba
t0<- (media1-mu1)/(sd1/sqrt(n1))
t0
#R/-1.2

#6 Obtener el valor de la distribución
tAlfa <- qt((alfa1/2),(n1-1), lower.tail = F)
tAlfa
#R/ 1.98

#7 Validar hipótesis
abs(t0)> tAlfa
#R/ false, se acepta la hipótesis, el peso es igual a 500 gramos

#.....Ejercicio 2
#H0: visitas = 40
#Hi: visitas >  40

alfa2 <- 0.01
n2 <- 8
media2 <- 42
sd2 <- 2
mu2 <-40

#5 Obtener el valor del estadístico de preuba
t<- (media2-mu2)/(sd2/sqrt(n2))
t
#R/2.83

#6 Obtener el valor de la distribución
tAlfa1 <- qt((alfa2/2),(n2-1), lower.tail = F)
tAlfa1
#R/ 3.49

#7 Validar hipótesis
abs(t)> tAlfa1
#R/ false, se acepta la hipótesis nula, en promedio realizan 40 visitas.
