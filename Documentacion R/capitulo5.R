library(TeachingDemos)
library(ggpubr)
#ingresar los datos
muestra <- c(19.33, 29.37, 29.14, 32.10, 25.04, 22.22, 31.26, 26.92,
             31.40, 17.66, 22.55, 20.69, 24.68, 28.74, 26.85, 29.68,
             29.27, 26.72, 27.08, 20.62)
#establecer datos conocidos
desv_est <- 2.32
n <- length(muestra)
valor_nulo <- 20


#crear grafico Q-Q para verificar la distribucion de la muestra
datos <- data.frame(muestra)
g <- ggqqplot(datos, x="muestra", color="SteelBlue")
print(g)

#verificar distribucion muestral usando la prueba de normalidad
#de Shapiro - Wilk
normalidad <- shapiro.test(muestra)
print(normalidad)

#fijar nivel de significacion
alfa <- 0.01

#calcular la media de la muestra
media <- mean(muestra)

#calcular estaditico de prueba 
Z <- (media - valor_nulo) / desv_est
cat("valor estidistico prueba", Z, "\n")

#calcular el valor p
p <- 2 * pnorm(Z, lower.tail = FALSE)
cat("p= ", p,"'n")

#hacer la prueba Z con R
prueba <- z.test(media, mu = valor_nulo, alternative = "two.sided",
                 stdev = desv_est, conf.level = 1-alfa)
print(prueba)
#------------------------------------------------------------------
#cargar los datos
tiempo <- c (411.5538 , 393.2753 , 445.8905 , 411.4022 , 498.8969 ,
             388.6731 , 430.0382 , 469.4734 , 409.5844 , 442.0800 ,
             418.1169 , 408.4110 , 463.3733 ,407.0908 , 516.5222)
#establecer datos conocidos
n_2 <- length(tiempo)
grados_libertad <- n_2 - 1
valor_nulo_2 <- 500

#verificar si la distribucion se acerca a la normal
g_2 <- ggqqplot(data=data.frame(tiempo),
                x="tiempo",
                color = "steelblue",
                xlab="Teorico",
                ylab="Muestra",
                title = "grafico Q-Q muestra v/s distr.normal")
print(g_2)

#fijar un nivel de significacion
alfa_2 <- 0.05
#calcular el estadistico de prueba
cat("\t prueba t para una muestra \n")
media_2 <- mean(tiempo)
des_v<-sd(tiempo)
error <- des_v /sqrt(n_2)
t <- (media_2 - valor_nulo_2)/error
cat("t= ", t, "\n")
#calcular el valor p
p <- pt(t,df = grados_libertad, lower.tail = TRUE)
cat("p= ", p,"\n")
#construir el intervalo de confianza
t_critico <- qt(alfa, df= grados_libertad, lower.tail= FALSE)
superior <- media_2 + t_critico *error
cat("intervalo de confianza = [-Inf", superior,"]\n", sep = " ")
#aplicar la prueba t de Student con la funcion R
prueba_t <- t.test(tiempo, 
                   alternative ="less",
                   mu = valor_nulo_2,
                   conf.level = 1-alfa_2)
print(prueba_t)

#-----------------------------------------------------------------
#prueba t para dos muestras pareadas
#cargar los datos
instancia <- seq(1,35,1)
t_A <- c (436.5736 , 470.7937 , 445.8354 , 470.9810 , 485.9394 ,
          464.6145 , 466.2139 , 468.9065 , 473.8778 , 413.0639 ,
          496.8705 , 450.6578 , 502.9759 , 465.6358 , 437.6397 ,
          458.8806 , 503.1435 , 430.0524 , 438.5959 , 439.7409 ,
          464.5916 , 467.9926 , 415.3252 , 495.4094 , 493.7082 ,
          433.1082 , 445.7433 , 515.2049 , 441.9420 , 472.1396 ,
          451.2234 , 476.5149 , 440.7918 , 460.1070 , 450.1008)

t_B <- c (408.5142 , 450.1075 , 490.2311 , 513.6910 , 467.6467 ,
          484.1897 , 465.9334 , 502.6670 , 444.9693 , 456.3341 ,
          501.1443 , 471.7833 , 441.1206 , 544.1575 , 447.8844 ,
          432.4108 , 477.1712 , 482.4828 , 458.2536 , 474.9863 ,
          496.0153 , 485.8112 , 457.4253 , 483.3700 , 510.7131 ,
          467.5739 , 482.5621 , 453.5986 , 385.9391 , 548.7884 ,
          467.2533 , 494.7049 , 451.9716 , 522.3699 , 444.1270)
diferencia <- t_A - t_B
#verificar si la distribucion se acerca a la normal
norm <- shapiro.test(diferencia)
print(norm)

#fijar un nivel de significacion
alpa <- 0.05
valor_nulo_3 <- 0
#aplicar la prueba t de Student a la diferencia de medias
prueba_A <- t.test(diferencia,
                   alternative="two.sided",
                   mu = valor_nulo_3,
                   conf.level = 1-alpa)
print(prueba_A)
#otra alternativa puede ser aplicar la prueba t de Student
#para dos muestras pareadas
prueba_B <- t.test(x=t_A,
                   y=t_B,
                   paired=TRUE,
                   alternative="two.sided",
                   mu=valor_nulo_3,
                   conf.level = 1-alpa)
print(prueba_B)
#conclusion
#La media de las diferencias esta dentro del intervalo de confianza y 
#ademas el valor p es mayor que el nivel de significacion, por lo que se falla al rechazar la hipotesis nula
# pero nuevamente, el resultado esta cerca del borde de significacion
#En consecuencia, se puede afirmar con 95% de confianza que pareciera
# no haber diferencia entre los tiempos de ejecucion de ambos algoritmos
#aunque seria necesario conseguir una muestra mas grande para tener mayor certeza


#-----------------------------------------------------------------
# Cargar los datos .
vacuna_A <- c(6.04 , 19.84 , 8.62 , 13.02 , 12.20 , 14.78 , 4.53 , 26.67 ,
               3.14 , 19.14 , 10.86 , 13.13 , 6.34 , 11.16 , 7.62)

vacuna_B <- c(5.32 , 3.31 , 5.68 , 5.73 , 4.86 , 5.68 , 2.93 , 5.48 , 6.10 ,
              2.56 , 7.52 , 7.41 , 4.02)

#verificar si las muestras se distribuyen de manera cercana a la normal
norm_VA <- shapiro.test(vacuna_A)
norm_VB <- shapiro.test(vacuna_B)
print(norm_VA)
print(norm_VB)
#fijamos el nivel de significacion 
alpha <- 0.01
#aplicar la prueba t para dos muestras independientes
prueba_ind <- t.test(x=vacuna_A,
                     y=vacuna_B,
                     paired = FALSE,
                     mu = 0,
                     conf.level = 1-alpha)
print(prueba_ind)
#calular la diferencia entre las medias
media_VA <- mean(vacuna_A)
media_VB <- mean(vacuna_B)
dif <- media_VA - media_VB
cat("diferencia de las medias=", dif,"[mg/ml\n")