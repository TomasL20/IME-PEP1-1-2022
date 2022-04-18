library(ggpubr)
#establecer la semilla para generar numeros aleatorios
set.seed(9437)

#generar aleatoriamente una poblacion de tamaño 1500
# (en este caso, con una distribucion cercana a la normal)

poblacion <- rnorm(n=1500, mean = 4.32, sd = 0.98)
#calcular la media de la poblacion
media_poblacion <- mean(poblacion)
cat("media de la poblacion", media_poblacion,"\n")

#tomar una muestra de tamaño 1250
tamano_muestra <- 1250
muestra <- sample(poblacion, tamano_muestra)

#calcular las medias acumuladas (es decir, con muestras de
#1,2, 3...elementos
n <- seq(along = muestra)
media <- cumsum(muestra) / n

#crear una matriz de datos con los tamaños y las medias muestrales
datos <- data.frame(n, media)

#graficar las medias muestrales
g <- ggline(data = datos,
            x = "n",
            y = "media",
            plot_type = "l",
            color = "blue",
            main = "media movil",
            xlab = "tamaño de la muestra",
            ylab = "media muestral")
#añadir al grafico una recta con la media de la poblacion
g <- g + geom_hline(aes (yintercept = media_poblacion),
                    color = "red", linetype = 2)
print(g)

#----------------------------------------------------------------

#establecer la semilla para generar numeros aleatorios
set.seed(94)

#generar aleatoriamente una poblacion de tamaño 1500
poblacion_1 <- rnorm(n=1500, mean = 4.32, sd = 0.98)
#calcular la media de la poblacion
media_poblacion_1 <- mean(poblacion_1)
cat("media de la poblacion", media_poblacion_1,"\n")

#tomar 1000 muestras de tamaño 100. quedan almacenadas
#como una matriz donde cada columna es una muestra
tam_muestra <- 100
repeticiones <- 1000
muestras_1 <- replicate(repeticiones, 
                        sample(poblacion_1, tam_muestra))

#calcular medias muestrales y almacenar los resultados en forma de 
#data frame
medias <- colMeans(muestras_1)
medias <- as.data.frame(medias)

#construir un histograma
g_1 <- gghistogram(data = medias,
                   x = "medias",
                   bins = 20,
                   title = "distribucion de la media muestral",
                   xlab = "media",
                   ylab = "frecuencias",
                   color = "blue",
                   fill = "blue",
                   alpha = 0.2)

#agregar linea vertical con la medida de la poblacion
g_1 <- g_1 + geom_vline(aes(xintercept = media_poblacion_1),
                        color = "red", linetype = 1)

print(g_1)


#-----------------------------------------------------------------
#calculo del valor p para una prueba de una cola
#generar una muestra donde la media cumpla con la hipotesis nula
set.seed(872)
media_poblacion_antiguo <- 530
media_muestra_nuevo <- 527.9
desvi_est <- 48
n <- 1600
error_est <- desvi_est / sqrt(n)

x<- seq(media_poblacion_antiguo - 5.2 * error_est,
        media_poblacion_antiguo + 5.2 * error_est,
        0.01)
y<- dnorm(x,mean=media_poblacion_antiguo, sd=error_est)
datos <- data.frame(x,y)
#graficar la muestra
g_2 <-ggplot(data = datos, aes(x))
g_2 <- g_2 + stat_function(fun = dnorm,
                           args = list(mean=media_poblacion_antiguo,
                                       sd=error_est),
                           colour = "steelblue", size = 1)
g_2 <- g_2 + ylab("")
g_2 <- g_2 + scale_y_continuous(breaks = NULL)
g_2 <- g_2 + scale_x_continuous(name = "tiempo de procesamiento [ms]")
g_2 <- g_2 + theme_pubr()

#Colorear el área igual o menor que la media observada .
g_2 <- g_2 + geom_area ( data = subset (datos ,
                                         x < media_muestra_nuevo ),
                           aes (y = y),
                           colour = " steelblue ",
                           fill = " steelblue ",
                           alpha = 0.5)

 # Agregar una línea vertical para el valor nulo .
g_2 <- g_2 + geom_vline(aes ( xintercept = media_poblacion_antiguo ),
                          color = " red", linetype = 1)

 print (g_2)

#Calcular el valor Z para la muestra .
 Z <- (media_muestra_nuevo - media_poblacion_antiguo ) / error_est

 # Calcular el valor p.
 p_1 <- pnorm (Z, lower.tail = TRUE )
 
 cat (" Valor p: ", p_1, "\n")
 
  # Tambi én se puede calcular el valor p directamente a partir de la
 # distribuci ón muestral definida por el valor nulo y el error
 # est á ndar .
 p_2 <- pnorm ( media_muestra_nuevo , mean = media_poblacion_antiguo ,
                  sd = error_est)
 
 cat (" Valor p: ", p_2)