library(discreteRV)
library(ggpubr)
#crear una variable discreta para representar el dado
#adulterado de la tabla 3.1
 resultados <- 1:6
 probabilidades <- c(0.25, 0.125, 0.125, 0.125, 0.125, 0.25)
 X <- RV( outcomes = resultados, probs = probabilidades)
 #calcular el valor esperado
 esperado <- E(X)
 cat("valor esperado:", esperado, "\n")
 
 #calcular la varianza
 varianza <- V(X)
 cat("varianza:", varianza, "\n")
 
 #calcular la desviacion estandar
 desviacion <- SD(X)
 cat("desviacion estandar",desviacion, "\n")
 #----------------------------------------------------------------
 #crear vector con los resultados de 5 lanzamientos del dado
 lanzar_5 <- SofIID(X,n=5)
 #crear vector con los resultados de 10 lanzamientos del dado
 lanzar_10 <- SofIID(X,n=10)
 #crear vector con los resultados de 20 lanzamientos del dado
 lanzar_20 <- SofIID(X,n=20)
 
 #graficar los resultados
 par(mfrow=c(1,3))
 plot(lanzar_5,
      main="lanzamiento de 5 dados",
      xlab= "suma de resultados",
      ylab = "probabilidad")
 plot(lanzar_10,
      main="lanzamiento de 10 dados",
      xlab= "suma de resultados",
      ylab = "probabilidad")
 plot(lanzar_20,
      main="lanzamiento de 20 dados",
      xlab= "suma de resultados",
      ylab = "probabilidad")
 
 #crear una variable aleatoria para un dado balanceado y calcular su valor
 #esperado, varianza y desviacion estandar
 
 Y <- RV(outcomes = resultados, probs = 1/6)
 esperado_y <- E(Y)
 varianza_y <- V(Y)
 desviacion_y <- SD(Y)
 cat("E(Y", esperado_y,"\n")
 cat("V(y)", varianza_y, "\n")
 cat("SD(y)", desviacion_y,"\n")
 
 #crear una combinacion lineal de variables aleatorias y calcular su valor
 #esperando, varianza y desviacion
 
Z <- 0.5 * X * 0.5 * Y
esperado_z <- E(Z)
varianza_z <- V(Z)
desviacion_z <- SD(Z)
cat("E(z)", esperado_z,"\n")
cat("V(z)", varianza_z, "\n")
cat("SD(z)", desviacion_z,"\n")
 

#generar valores para una distribucion normal con media 0 y desviacion estandar 1
media <- 0
desv_est <- 1
x <- seq(-15, 35, 0.01)
y <- dnorm (x, mean=media, sd=desv_est)
normal_1 <- data.frame(x,y)

#generar valores para una distribucion normal con media 10 y desviacion estandar 6
media <- 10
desv_est <- 6
x <- seq(-15, 35, 0.01)
y <- dnorm (x, mean=media, sd=desv_est)
normal_2 <- data.frame(x,y)

#graficar ambas funciones 
g <- ggplot (normal_1, aes(x,y)) + geom_line(color = "blue")
g <- g + geom_line(data = normal_2, color = "red")
g <- g + theme_pubr()
print(g)
#-----------------------------------------------------------------
library(dplyr)
#cargar conjunto de datos
datos <- mtcars
#renombrar columnas
datos <- datos %>% rename(Rendimiento = mpg, Cilindrada = cyl,
                          Desplazamiento = disp, Potencia = hp,
                          Eje = drat, Peso = wt, Cuarto_milla = qsec,
                          Motor = vs, Transmision = am, Cambios = gear,
                          Carburadores = carb)
#grafico Q-Q para la variable Rendimiento
g_r <- ggqqplot(datos,
                x ="Rendimiento",
                color = "red")
print(g_r)