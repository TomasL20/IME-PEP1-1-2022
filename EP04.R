library(dplyr)

#lectura archivo

dir <- "c:/Users/nombre/Downloads"   #cambiar dependiendo de la dirección de su archivo
basename <- "EP04-datos.csv"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)

# nivel significacion utilizado en los problemas
alfa <- 0.05



#Problema 1
#El Comite Olimpico cree que el mejor tiempo medio de los atletas blancos 
#despues de ingresar al programa de entrenamiento es superior a 11,78 segundos.
#ï¿Soportan los datos esta afirmacion?

#H0: el tiempo medio de los atletas blancos despuÃ©s de ingresar al programa de
#    entrenamiento es igual a 11,78 segundos.
#H1: el tiempo medio de los atletas blancos despuÃ©s de ingresar al programa de
#    entrenamiento es superior a 11,78 segundos.

#Formula matematica de las hipotesis:
#H0: mu = 11,78 segundos
#H1: mu > 11,78 segundos

#Se filtran los datos para obtener solo a los atletas de raza blanca.
atletas.blancos <- datos %>% filter(Raza == "Blanca")
#Se obtiene la columna que contiene la mejor marca post entrenamiento de los
#atletas.
marcas.post.entrenamiento <- atletas.blancos$Posterior

#Debido a que la cantidad de observaciones de la muestra es menor a 30 no 
#se puede aplicar una prueba Z, por lo que se utiliza una prueba t de 
#Student para una muestra.

#Como cada observacion corresponde a un individuo diferente estas son 
#independientes entre si.

#Se realiza la prueba de Shapiro-Wilk para comprobar si la muestra sigue
#una distribucion cercana a la normal.
normalidad <- shapiro.test(marcas.post.entrenamiento)
print(normalidad)

#Dado que el p-valor de la prueba de Shapiro-Wilk es mucho mayor al nivel
#de significacion definido se sabe que la muestra sigue una distribucion 
#cercana a la normal.

#Con lo anterior se cumplen las dos condiciones necesarias para realizar
#la prueba t para una muestra.
prueba1 <- t.test(marcas.post.entrenamiento, 
                  alternative = "greater",
                  mu = 11.78,
                  conf.level = 1 - alfa)

#Revisando los resutados obtenidos de la prueba se puede observar que el 
#p-valor arrojado es igual a 0.005727791, el cual es muy inferior al nivel
#de significacion definido al inicio del problema, por lo que se rechaza
#hipotesis nula en favor de la hipotesis alternativa, es decir, que el 
#tiempo medio de los atletas blancos despues de ingresar al programa de
#entrenamiento es superior a 11,78 segundos.

#Problema 2
#¿Sugieren los datos que la mejor marca de los atletas negros se reduce en 
#4,68 segundos tras el entrenamiento?

#H0: la media de la diferencia de las mejores marcas antes y despues del
#    entrenamiento de los atletas negros es igual a 4,68 segundos.
#H1: la media de la diferencia de las mejores marcas antes y despues del
#    entrenamiento de los atletas negros no es igual a 4,68 segundos.

#Formula matematica de las hipotesis:
#H0: mu = 4,68 segundos
#H1: mu != 4,68 segundos

# atletas negros
atletas.negros <- datos %>% filter(Raza == "Negra")
# atletas negros previo entrenamiento
atletasNegrosPrevio <- atletas.negros$Previo
# atletas negros post entrenamiento 
atletasNegrosPosterior <- atletas.negros$Posterior

# en este caso tenemos los datos para 2 muestras pareadas

diferencia <- atletasNegrosPrevio - atletasNegrosPosterior

# verificamos si la distribucion se acerca a la normal 

normalidad <- shapiro.test(diferencia)
print(normalidad)

#w = 0.96 , p-value = 0.6622

# como p-valor = 0.6622 > alpha = 0.05 , no se rechaza H0 por lo que la variable presenta un comportamiento normal 


# aplicamos la prueba t a las muestras pareadas

pruebaT <- t.test(x = atletasNegrosPrevio, 
                  y = atletasNegrosPosterior, 
                  paired = TRUE, 
                  alternative = "two.sided", 
                  mu = 4.68, 
                  conf.level = 1 - alfa)

print(pruebaT)

# t = -1.5172, df = 17, p-value = 0.1476
# intervalo de confianza 95% = [2.813041;4.985058]
# media de las diferencias = 3.89905

# media de las diferencias se encuentra dentro del intervalo de confianza
# p-value = 0.1476 > alpha = 0.05 -> no rechazar H0 

# respuesta 2 : 
# efectivamente los datos sugieren que la mejor marca de los atletas negros se reduce en 4,68 segundos tras el entrenamiento

#Problema 3
#¿Es posible afirmar que, en promedio, los atletas negros superan a los 
#blancos por menos de 1,34 segundos antes del entrenamiento?

#H0: los atletas negros superan en 1,34 segundos a los atletas blancos antes
#    del entrenamiento.
#H1: los atletas negros superan en menos de 1,34 segundos a los atletas 
#    blancos antes del entrenamiento.

#Formula matematica de las hipotesis:
#muA = media de las marcas de los atletas negros.
#muB = media de las marcas de los atletas blancos.
#H0: muA - muB = 1,34 segundos
#H1: muA - muB < 1,34 segundos

#Obtenemos los datos necesarios
#En esta ocasion necesitamos los datos de los atletas blanco y negros
datosBlancos <- datos %>% filter(Raza == "Blanca")
datosNegros <- datos %>% filter(Raza == "Negra")

#Obtenemos la diferencia entre los tiempos
#Debido a que en ocasiones el tiempo previo es menor al posterior, se presentan
# datos negativos. Como no se sabe que realizar los datos se trabajaran en bruto y se 
# asumira ese error
datosBlancos <- datosBlancos %>% mutate(diferenciaB = Previo - Posterior)
datosNegros <- datosNegros %>% mutate(diferenciaN = Previo - Posterior)

#Al comprobar que cada uno de los tiempos es independiente, ya se cumple para primera
# condicion para la prueba t

#Ahora debemos determinar si las observaciones son cercanas a la normal
normalidad.B <- shapiro.test(datosBlancos$diferenciaB)
print(normalidad.B)
normalinad.N <- shapiro.test(datosNegros$diferenciaN)
print(normalinad.N)

#Ya que el p-valor es p = 0.8791 para los datos blancos y
# p = 0.6622 para los datos negros, ambos valores son bastantes altos
# por lo que podemos concluir que ambas muestras provienen de una poblacion que
# se distribuye de manera normal

#Aplicamos la prueba t para dos muestras independientes
prueba3 <- t.test(x = datosBlancos$diferenciaB,
                  y = datosNegros$diferenciaN,
                  paired = FALSE,
                  alternative = "less",
                  mu = 1.34,
                  conf.level = 1 - alfa)
print(prueba3)

#Con lo anterior tenemos que el p-valor corresponde a 
# 0.001885, esto es inferior al alfa definido en 0.05
# Lo que significa que la evidencia en favor de H1 es fuerte,
# por lo que rechazamos la hipotesis nula, por lo que podemos
# concluir con un 95% de confianza que los atletas negros
# superan en menos de 1.34 segundos a los atletas blancos