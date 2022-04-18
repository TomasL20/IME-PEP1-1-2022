#Equipo 7

#Integrantes:
#Tomas Lopez
#Adolfo Navarrete
#Fabian Sepulveda

library(ggpubr)
library(gtools)
##PARTE 1##

#Lectura de archivo 
poblacion <- read.csv2("C:\\IME\\EP03\\Casen 2017.csv", encoding = "UTF-8")

#tamaño de la población
tamano <- nrow(poblacion)

#ingresos de la población
ingreso <- as.numeric(poblacion[["ytot"]])

#Limitador
poda <- 0.2

#Quintil 20%
q20 <- quantile(ingreso, poda)

#Quintil 80%
q80 <- quantile(ingreso, 1 - poda)

#Ingresos según los quintiles
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]

#tamaño de la lectura
tamano.podado <- length(ingreso.podado)

#media de los ingresos
media.ingreso <- mean(ingreso.podado)

#Desviación estandar de los ingresos
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamano.podado )

#Definicion de la semilla
set.seed(266)

#Generamos la distribucion normal
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#Generamos la distribucion z
ingreso.z <- (ingreso.normal-media.ingreso)/sd.ingreso

hist(ingreso.z, prob = TRUE)

#Generamos chi-cuadrado

#Funcion que toma un conjunto segun el grado de libertad y realiza la suma al cuadrado
vector.chi <- function(i,Z,grados){
  sum(sample(Z, grados, replace = FALSE)^2)
}

#Declaramos las dos distribuciones chi
ingreso.chi4 <- sapply(1:length(ingreso.z),vector.chi, ingreso.z, 4)
ingreso.chi14 <- sapply(1:length(ingreso.z),vector.chi, ingreso.z, 14)

hist(ingreso.chi4)
hist(ingreso.chi14)

#Generamos la distribucion F

ingreso.F <- (ingreso.chi4/4)/(ingreso.chi14/14)

hist(ingreso.F, prob = TRUE)

## PARTE 2 ##

#La semilla corresponde a la misma que la parte 1

#Numero de repeticiones

#Bernoulli
#
n.repeticiones <- 50

ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)

vector.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Binomial
n.repeticiones <- 50

#Se genera la distribuci�n binomial
#para ello se crea una funcion donde se toman 15 datos del vector
# y se sumaran si es que se presenta alguna mujer
binomial.exitos <- function(i, k) 
  sum(sample(vector.repeticiones, k, replace = TRUE))

binomial <- sapply(1:2000, binomial.exitos, k = 15)

hist(binomial, prob = TRUE)

#Geometrica
#Se genera la distribuci�n geom�trica
#para ello se crea una funcion donde dado el vector de repeticiones, tomaremos 15 datos
# y segun aquel dato que coincida se entregara
#Se usa match para tomar aquel dato que coincida
geometrica.exito <- function(i, k) 
  match(1,sample(vector.repeticiones, k, replace = TRUE))

geometrica <- sapply(1:2000, geometrica.exito, k = 15)

hist(geometrica)