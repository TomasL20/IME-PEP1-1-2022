library(ggpubr)
library(tidyverse)
#generar un vector con un rango de valores para la efecto
#de medias
efecto <- seq(-2.5,2.5,0.01)

#calcular el poder para una prueba t bilateral, para cada tamaño
#del efecto, asumiendo una muestra con desviacion estandar igual a 1
#se consideran 4 escenarios para calcular el poder:
#1. Una muestra de tamaño 6 y nivel de significacion 0.05
#2. Una muestra de tamaño 6 y nivel de significacion 0.01
#3. Una muestra de tamaño 10 y nivel de significacion 0.05
#4. Una muestra de tamaño 10 y nivel de significacion 0.01

n_6_alfa_05 <- power.t.test(n=6,
                            delta = efecto,
                            sd=1,
                            sig.level = 0.05,
                            type = "one.sample",
                            alternative = "two.sided")$power

n_6_alfa_01 <- power.t.test(n=6,
                            delta = efecto,
                            sd=1,
                            sig.level = 0.01,
                            type = "one.sample",
                            alternative = "two.sided")$power

n_10_alfa_05 <- power.t.test(n=10,
                            delta = efecto,
                            sd=1,
                            sig.level = 0.05,
                            type = "one.sample",
                            alternative = "two.sided")$power

n_10_alfa_01 <- power.t.test(n=10,
                            delta = efecto,
                            sd=1,
                            sig.level = 0.01,
                            type = "one.sample",
                            alternative = "two.sided")$power
#construir matriz de datos en formato ancho
datos <- data.frame(efecto, n_6_alfa_05, n_6_alfa_01,
                    n_10_alfa_05, n_10_alfa_01)
#llevar a formato largo
datos <- datos %>% pivot_longer(!"efecto",
                                names_to = "fuente",
                                values_to = "poder")
#formatear fuente como variable categorica
niveles <- c("n_6_alfa_05", "n_6_alfa_01",
             "n_10_alfa_05", "n_10,alfa_01")

etiquetas <- c("n=6, alfa =0 ,05", "n=6, alfa =0 ,01", "n=10 , alfa =0 ,05",
               "n=10 , alfa =0 ,01")

datos [[" fuente "]] <- factor ( datos [[" fuente "]], levels = niveles ,
                                  labels = etiquetas )

#graficar las curvas de poder
g <- ggplot (datos, aes(efecto, poder, colour = factor(fuente)))
g <- g + geom_line()
g <- g + labs(colour = "")
g <- g + ylab("poder estadistico")
g <- g + xlab ("tamaño del efecto")
g <- g + scale_color_manual (values=c("red","blue","chartreuse4",
                                      "orange"))
g <- g + theme_pubr()
g <- g + ggtitle("curvas de poder para prueba t bilateral")
g <- g + geom_vline(xintercept = 0, linetype = "dashed")
print(g)
#--------------------------------------------------------------------

#generar un vector con un rango para el tamaño de la muestra
n <- seq(5,8000,5)
#definir constantes
desv_est <- 6
alfa <- 0.05
tam_efecto <- 0.5

#se calcula el poder con que se detecta el tamaño del efecto para
#cada tamaño de la muestra, asumiendo una prueba bilateral para
#una sola muestra

poder <- power.t.test(n=n,
                    delta = tam_efecto,
                    sd = desv_est,
                    sig.level = alfa,
                    type = "two.sample",
                    alternative = "two.sided")$power
#crear un data frame
datos_2 <-data.frame(n, poder)

#graficar la curva de poder
g_2 <- ggplot(datos_2, aes(n,poder))
g_2 <- g_2 + geom_line(colour="red")
g_2 <- g_2 + ylab ("poder estadistico")
g_2 <- g_2 + xlab("tamaño de la muestra")
g_2 <- g_2 + theme_pubr()
g_2 <- g_2 + ggtitle("relacion entre el poder y el tamaño de la muestra")
print(g_2)

#-------------------------------------------------------------------
#calculo teorico del poder
library(pwr)
#fijar valores conocidos
sigma <-12
alpha<-0.05
n <- 36
media_nula <- 0
#calcular el error estandar
SE <- sigma/sqrt(n)
#graficar la distribucion muestral de la media de las diferencias si
#la hipotesis nula fuera verdadera

x <- seq(-6,SE, 4*SE, 0.01)
y <- dnorm(x, mean=media_nula, sd = SE)
g <- ggplot(data = data.frame(x,y), aes(x))
g <- g + stat_function(
  fun = dnorm,
  args = list(mean=media_nula, sd=SE),
  colour = "red", size = 1)
g <- g + ylab(" ")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Diferencia en tiempos de ejecuci ón [ms]",
                               breaks  = seq (-6, 4, 2))
g <- g + theme_pubr ()

# Colorear la región de rechazo de la hipó tesis nula .

Z_critico <- qnorm ( alfa /2, mean = media_nula , sd = SE , lower.tail = FALSE )
q_critico_inferior <- media_nula - Z_critico
q_critico_superior <- media_nula + Z_critico

 g <- g + geom_area ( data = subset(df , x < q_critico_inferior ),
                      aes (y = y),
                      colour = "red ",
                      fill = "red",
                      alpha = 0.5)

 g <- g + geom_area ( data = subset(df , x > q_critico_superior ),
                      aes (y = y),
                      colour = "red ",
                      fill = "red",
                      alpha = 0.5)

print(g)

# Superponer la distribuci ón muestral de la media de las diferencias
# si la la diferencia de medias fuera -4.
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_efecto , sd = SE),
  colour = " blue ", size = 1)

 # Colorear la regi ón de la nueva curva situada en la regi ón de
 # rechazo de la curva original .
 x1 <- seq (-6 * SE , 4 * SE , 0.01)
 y1 <- dnorm (x, mean = media_efecto , sd = SE)
 g <- g + geom_area ( data = subset ( data.frame (x1 , y1),
                                           x < q_critico_inferior ),
                          aes (x = x1 , y = y1),
                          colour = " blue ",
                          fill = " blue ",
                          alpha = 0.5)

 g <- g + geom_area ( data = subset ( data.frame (x1 , y1),
                                     x > q_critico_superior ),
                          aes (x = x1 , y = y1),
                          colour = " blue ",
                          
                           fill = " blue ",
                           alpha = 0.5)
print (g)

# Calcular el poder de acuerdo al aná lisis teó rico .
poder <- pnorm (q_critico_inferior ,
                    mean = media_efecto ,
                    sd = SE ,
                    lower.tail = TRUE )
+ pnorm (q_critico_superior ,
             mean = media_efecto ,
             sd = SE ,
            lower.tail = FALSE )

 cat (" Poder = ", poder , "\n")
 # Calcular la probabilidad de cometer un error tipo II.
 beta <- 1 - poder_teorico
 cat (" Beta = ", beta , "\n")
#-------------------------------------------------------------
library (pwr)
  #fijar valores conocidos
 n <- 36
 diferencia <- 4
 des_est <- 12
 alfa <- 0.05
 poder <- 0.9
 
#calcular el poder usando la funcion power.t.test()
 cat("calculo del poder con power.t.test () \n")
 resultado_poder <- power.t.test(n=n,
                                 delta = diferencia,
                                 sd=des_est,
                                 sig.level = alfa,
                                 power = NULL,
                                 type = "paired",
                                 alternative = "two.sided"
                                 )
 print(resultado_poder)
 #calculo del tamaño de la muestra usando la funcion power.t.test()
 cat("calculo del tamaño de la muestra con power.t.test()")
 resultado_n <- power.t.test (n=NULL,
                              delta = diferencia,
                              sd= des_est,
                              sig.level = alfa,
                              power = poder,
                              type = "paired",
                              alternative = "two.sided")
 
 n <- ceiling(resultado[["n"]])
 cat("n= ", n,"\n")
 #calcular el tammaño del efecto (d de Cohen)
 d <- (4/des_est) * ((n-2)/n-1.25)
 
 #calcular el poder usando la funcion pwr.t.test()
 cat("calculo del poder con pwr.t.test () \n")
 resultado_pwr <- pwr.t.test(n=n,
                             d=d,
                             sig.level = alfa,
                             power = NULL,
                             type = "paired",
                             alternative= "two.sided")
 print(resultado_pwr)
 
 #calculo del tamaño de la muestra usando la funcion pwr.t.test()
 cat("calculo del tamaño de la muestra usando la funcion pwr.t.test()")
 resultado_tamn <- pwr.t.test(n=NULL,
                              d=d,
                              sig.level = alfa,
                              power = poder,
                              type = "paired",
                              alternative = "two.sided")
 n<- ceiling(resultado_tamn[["n"]])
 cat("n= ", n,"\n")
 
 
 
 
