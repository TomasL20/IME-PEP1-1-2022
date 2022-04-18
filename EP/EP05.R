#Equipo 7
library(ggpubr)
library(pwr)
#Enunciado:

#Se sabe que una maquina que envasa detergentes industriales llena bidones con un 
#volumen de producto que sigue una distribucion normal con desviacion estandar de 
#1 litro. Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de 
#la planta requiere determinar si la maquina esta llenando los bidones con una 
#media de 10 litros.

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 1 ====
#====================

#Si el ingeniero piensa rechazar la hipotesis nula cuando la muestra presente
#una media menor a 9,8 litros o mayor a 10,2 litros, ¿cual es la probabilidad de
#que cometa un error de tipo I?

#Datos iniciales:

sd <- 1
n <- 100
media_nula <- 10

errorEst <- sd/sqrt(n)

#nivel de signficacion
alpha <- 0.01


#Debemos calcular el valor
valorZ_Critico <- qnorm(alpha/2, mean = media_nula, sd = errorEst, lower.tail = FALSE)

x <- seq(10 - valorZ_Critico*errorEst, 10 + valorZ_Critico*errorEst, 0.01)
y <- dnorm(x, mean = media_nula, sd = errorEst)

#Data frame
datos <- data.frame(x,y)

g1 <- ggplot(data = datos, aes(x))

g1 <- g1 + stat_function(
  fun = dnorm,
  args = list(mean = media_nula, sd = errorEst),
  colour = "red" ,
  size = 1
)


area_inferior = pnorm(9.8 , mean = media_nula, sd = errorEst, lower.tail = TRUE)
area_superior = pnorm(10.2 , mean = media_nula, sd = errorEst, lower.tail = FALSE)
area <- (area_inferior + area_superior)*100
g1 <- g1 + geom_area(data = subset(datos, x < 9.8), aes(y = y),
                                   colour = "red", fill = "red", alpha = 0.5)
g1 <- g1 + geom_area(data = subset(datos, x > 10.2), aes(y = y),
                                   colour = "red", fill = "red", alpha = 0.5)
g1 <- g1 + theme_pubr()

print(g1)

#====================
#==== Respuesta 1 ===
#====================

# La probabilidad de que se cometa un error de tipo I, cuando la muestra presente una media menor
#a 9.8 litros o mayor a 10.2 litros es del 4,55%

cat("La probabilidad de que el ingeniero cometa un Error de tipo I es del: ",area, "%")

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 2 ====
#====================

# Si el verdadero volumen medio de los bidones fuera de 10,1 litros, Â¿cuÃ¡l seria 
#la probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa 
#un error de tipo II?

mu2 <- 10.1

valorz_2 <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)
x1 <- seq(10 - valorz_2*errorEst, 10 + valorz_2*errorEst, 0.01)
y1 <- dnorm(x1, mean = mu2, sd = errorEst)

g1 <- g1 + geom_area(data = subset(data.frame(x1,y1),
                                   x < area_inferior),
                     aes(x = x1, y = y1),
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)

g1 <- g1 + geom_area(data = subset(data.frame(x1,y1),
                                   x > area_superior),
                     aes(x = x1, y = y1),
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)


print(g1)

poder <- pnorm(area_inferior, mean = mu2, sd = errorEst, lower.tail = TRUE) + pnorm(area_superior, mean = mu2, sd = errorEst, lower.tail = FALSE)

beta <- 1 - poder

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 3 ====
#====================

#Como no se conoce el verdadero volumen medio, genere un grafico del poder 
#estadistico con las condiciones anteriores, pero suponiendo que el verdadero
#volumen medio podria variar de 9,6 a 10,4 litros.

#Como la diferencia de entre las medias es de 0.1, se genera una secuencia en 
#base a este valor.
tam_efecto <- seq(-1,1,0.1)

#Calculo del poder de acuerdo a los datos entregados en el enunciado
poder <- power.t.test(n = 100,
                      delta = tam_efecto,
                      sd = 1,
                      sig.level = 0.016, #En base al calculo de la pregunta 1
                      type = "one.sample",
                      alternative = "two.sided")$power

#data frame
datos <- data.frame(x = tam_efecto, y = poder)

#Grafico de la curva de poder
g3 <- ggplot(data = datos, aes(tam_efecto, poder))
g3 <- g3 + geom_line()
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Poder Estadístico")
g3 <- g3 + xlab("Tamaño del efecto [Litros]")

g3 <- g3 + theme_pubr()
g3 <- g3 + ggtitle("Gráfico de poder estadístico bilateral")
g3 <- g3 + geom_vline(xintercept = 0, linetype = "dashed")

#====================
#==== Respuesta 3 ===
#====================

#Se imprime el grafico del poder estadistico generado anteriormente.
print(g3)

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 4 ====
#====================

#Considerando un volumen medio de 10 litros, ¿cuantos bidones deberian revisarse
#para conseguir un poder estadistico de 0,7 y un nivel de significacion de 0,05?

#Media nula 
mu0 <- 10
#Media muestral
mu <- 10.1
#Se calcula la d de Cohen para una prueba t de una muestra usando la formula
d_cohen <- (mu - mu0)/sd

#Se aplica la funcion pwr.t.test con el valor n en NULL, ya que es el valor que se
#desea obtener, los demas parametros se rellenan con la d de Cohen obtenida, el 
#nivel de significacion y el poder estadistico que se desea lograr, junto con los
#parametros que indican que la prueba es de una muestra y es bilateral.
cantidad_bidones <- pwr.t.test(n = NULL,
                               d = d_cohen,
                               sig.level = 0.05,
                               power = 0.7,
                               type = "one.sample",
                               alternative = "two.sided")

#====================
#==== Respuesta 4 ===
#====================

#De lo anterior, se obtiene el valor n necesario para lograr el nivel de significacion
#igual a 0.05 y un poder estadistico igual a 0.7.
#El n obtenido es 619.1235, que se redondea hacia arriba, por lo que se deberian
#revisar 620 bidones para conseguir los valores deseados.

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 5 ====
#====================

#¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de
#cometer un error de tipo I a un 1% solamente?

#Para obtener la cantidad de bidones que se debe revisar para que la probabilidad de
#cometer un error de tipo I sea 1% se aplica la funcion pwr.t.test con los mismos
#parametros que en la pregunta anterior, cambiando el sig.level a 0.01.
cantidad_bidones2 <- pwr.t.test(n = NULL,
                                d = d_cohen,
                                sig.level = 0.01,
                                power = 0.7,
                                type = "one.sample",
                                alternative = "two.sided")

#====================
#==== Respuesta 5 ===
#====================

#De la funcion anterior se obtiene que la cantidad de bidones que se deben revisar 
#es de 964.4612, que se redondea a 965 bidones.