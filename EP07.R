#Importacion de librerias.
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

#====================
#==== Pregunta 1 ===
#====================

# En su desquiciada investigacion para acabar con los vampiros, Van Helsing ha descubierto que sus
# enemigos tienen predileccion por la sangre humana tipo AII+. El cazador sospecha que estos monstruos
# tienen preferencia por la sangre de los adultos, pero aun no esta convencido. Por esta razon, mediante
# artimanas, ha encerrado a 14 ninos y 20 adultos con este tipo de sangre en un reducto de vampiros. Tras
# 3 noches, 4 de los ninos y 11 de los adultos fueron atacados. ¿Existe relacion entre el ataque de vampiros
# y la edad de la victima?

#Para comprobar si la cantidad de ataques esta relacionado con la edad de las victimas se realizara una prueba
#exacta de Fisher, la que permite comprobar si estas variables son independientes o si estan relacionadas.

#HIPOTESIS
# H0 : La cantidad de ataques es independiente de la edad de las victimas.
# HA : La cantidad de ataques esta relacionada con la edad de las victimas.

#Se crea la tabla requerida por la funcion que realiza la prueba de Fisher.
edad <- c(rep("Nino",14),rep("Adulto",20))
resultado <- c(rep("Atacado",4),rep("No-Atacado",10),rep("Atacado",11),rep("No-Atacado",9))
datos <- data.frame(resultado, edad)
tabla1 <- xtabs( ~. , datos)

#Se imprime la tabla por consola para ver que esta correcta.
print(tabla1)

#Se fija el nivel de significacion (alfa) en 0.05.
alfa <- 0.05

#Se realiza la prueba exacta de Fisher con la funcion fisher.test de R.
pruebafisher <- fisher.test(tabla1, 1-alfa)

#Se imprime el resultado de la prueba por consola.
print(pruebafisher)

#====================
#==== Respuesta 1 ===
#====================

#De la prueba anterior se obtiene un p-valor igual a 0.1706, el cual es mayor al nivel de significacion fijado,
#por lo que se falla al rechazar la hipotesis nula, concluyendo con un 95% de confianza que la cantidad de
#ataques no esta relacionada con la edad de las victimas.

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 2 ===
#====================

# Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan con elevados niveles de
# ansiedad. Para ello, han decidido evaluar un nuevo programa de bienvenida que busca facilitar la
# adaptacion a la vida universitaria. Para ello, han reclutado a un grupo de 15 voluntarios a quienes se les
# midio el nivel de ansiedad (alto o bajo) antes y despues de participar en el programa de bienvenida:
#   - 2 estudiantes no presentaron ansiedad ni antes ni despues.
#   - 9 estudiantes inicialmente ansiosos dejaron de estarlo.
#   - 3 estudiantes mantuvieron un elevado nivel de ansiedad.
#   - El estudiante restante desarrollo sintomas de ansiedad tras participar en el programa.
# ¿Que se puede concluir acerca del nuevo programa de bienvenida?

#Para este ejercicio se decidio realizar una prueba de mcNemar, la cual sirve para comprobar si existen cambios
#significativos en dos mediciones distintas de una variable dicotomica, queriendo en este caso comprobar si hubo
#cambios en el nivel de ansiedad de los estudiantes luego de aplicar el programa de bienvenida.

#HIPOTESIS
#H0 : NO hay cambios significativos en la cantidad de estudiantes con ansiedad alta luego de aplicar
#     el programa.
#H1 : SI hay cambios significativos en la cantidad de estudiantes con ansiedad alta luego de aplicar
#     el programa.

#Se fija el nivel de significacion para esta prueba en 0.05.
alfa <- 0.05

#Se crea la tabla con los datos que se necesitan para realizar la prueba.
despues <- c(rep("Ansiedad alta",4), rep("Ansiedad baja",11))
antes <- c(rep("Ansiedad alta",3),rep("Ansiedad baja",3),rep("Ansiedad alta",9))
tabla2 <- table(antes,despues)

#Se imprime la tabla por consola para comprobar que esta correcta.
print(tabla2)

#Se realiza la prueba de mcNemar usando la funcion mcnemar.test de R, que recibe la tabla creada anteriormente.
pruebamcnemar <- mcnemar.test(tabla2)

#Se imprime por consola el resultado obtenido de la prueba.
print(pruebamcnemar)

#====================
#==== Respuesta 2 ===
#====================

#De la prueba anterior se obtiene un p-valor igual a 0.02686, el cual es menor al nivel de significacion fijado,
#por lo que se rechaza la hipotesis nula en favor de la hipotesis alternativa, concluyendo con un 95% de confianza 
#que si hubo cambios significativos en la cantidad de estudiantes con ansiedad alta luego de pasar por el programa
#de bienvenida.

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 3 ===
#====================

# En noviembre de 2019, se realizo un estudio acerca de la aprobacion al presidente Sebastian Pinera entre
# 440 profesores y estudiantes de una prestigiosa universidad, obteniendose los resultados que se muestran
# en la tabla. ¿Son similares las opiniones de ambos segmentos de la comunidad universitaria?

#Para comparar las opiniones de los estudiantes y los profesores se decidio realizar una prueba chi-cuadrado
#de homogeneidad, la que nos permite comprobar si las proporciones de ambos grupos son similares o no.

#Sobre los requisitos que se deben cumplir para realizar la prueba de chi-cuadrado, se tiene que como cada muestra
#representa a una persona distinta, estas son independientes entre si. Para comprobar la segunda la condicion, de que
#hayan mas 5 de muestras esperadas en cada grupo se calculo la cantidad esperada usando la formula Eij = (ni * nj) / n.
#Los valores obtenidos se pueden ver en la siguiente tabla:

estudiantesEsperados <- c(32.5,215.68,11.81)
profesoresEsperados <- c(22.5,149.32,8.19)
tablaE <- as.table(rbind(estudiantesEsperados, profesoresEsperados))
dimnames(tablaE) <- list(c("estudiantes esperados","profesores esperados"),
                         c("Aprueba","Desaprueba","Ninguna"))
print(tablaE)

#En esta tabla se puede ver que para cada categoria en cada grupo se esperan mas de 5 muestras, por lo que se cumple 
#la condicion.

#HIPOTESIS:
#H0 : Ambos segmentos de la comunidad universitaria tienen opiniones similares sobre el presidente Sebastian Pinera
#H1 : Ambos segmentos de la comunidad universitaria tienen opiniones distintas sobre el presidente Sebastian Pinera

#Fijaremos un nivel de confianza del 95% (alpa = 0.05).
alfa <- 0.05

#Vectores con los datos de las opiniones para la tabla.
estudiantes <- c(35,208,17)
profesores <- c(20,157,3)

#Se crea la tabla con los vectores.
tabla3 <- as.table(rbind(estudiantes, profesores))

#Ajustamos los nombres de las filas y columnas.
dimnames(tabla3) <- list(c("estudiantes","profesores"),
                          c("Aprueba","Desaprueba","Ninguna"))
#Mostramos la tabla por pantalla para comprobar que este correcta.
print(tabla3)

#Se realiza la prueba chi-cuadrado de homogeneidad usando la funcion chisq.test de R.
pruebachi <- chisq.test(tabla3)

#Se muestran los resultados de la prueba por consola.
print(pruebachi)

#====================
#==== Respuesta 3 ===
#====================
#De la prueba anterior se obtiene un p-valor igual a 0.03521, el cual es menor al nivel de significacion fijado,
#por lo que se rechaza la hipotesis nula en favor de la hipotesis alternativa, concluyendo con un 95% de confianza 
#que ambos segmentos tienen opiniones distintas sobre el presidente Sebastian Pinera.

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 4 ===
#====================

# La Facultad de Ingenieria desea saber si existe diferencia significativa en el desempeno de los estudiantes
# en asignaturas criticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
# asignaturas, indica si una muestra de 50 estudiantes aprobo o reprobo. ¿Que puede concluir la Facultad?
# Indicacion: obtenga la muestra a partir del archivo "EP07 Datos.csv" que se encuentra en el directorio
# compartido, usando la semilla 555. Considere un nivel de significación = 0,05.

#Para este problema se realizara una prueba Q de Cochran porque se compararan las proporciones de aprobacion en
#3 ramos distintos, y es esta prueba la que nos permmite hacerlo.

#Condiciones:
#1. Se cumple que la variable de respuesta es dicotomica, en este caso puede tomar los valores aprobado y reprobado.
#2. Se cumple que la variable independiente sea categorica, porque la variable ramo solo puede tomar 3 valores 
#   (calculo, algebra o fisica).
#3. Se cumple que las observaciones son independientes entre si ya que cada una corresponde a una persona distinta y
#   a ramos distintos, ademas las observaciones de la muestra se obtuvieron aleatoriamente.
#4. Se cumple que el tamano de la muestra sea los suficientemente grande, ya que con una muestra de 50 observaciones
#   y 3 niveles en la variable independiente, por lo que n * k >= 24 (n*k = 50 * 3 = 150).

#HIPOTESIS:
#H0 : La proporcion de aprobacion es la misma para todos los ramos
#H1 : La proporcion de aprobacion es distinta para al menos un ramos

#Se fija un nivel de significacion de 0.05.
alfa <- 0.05

#Cambiar dependiendo de la dirección de su archivo
dir <- "C:/Users/adolf/Desktop/Cosas/Progra/IME"   
basename <- "EP07 Datos.csv"
file <- file.path(dir, basename)

#Se obtienen los datos contenidos en el archivo EP07 Datos.csv
datos4 <- read.csv2(file = file)

#Se fija la semilla de la muestra en 555, la que se pide en el enunciado
set.seed(555)

#Se obtiene una muestra aleatoria de 50 observaciones desde los datos del archivo.
datos4 <- sample_n(datos4 ,50)

#Se transforma la matriz de datos a formato largo.
datos4 <- datos4 %>% pivot_longer(c("Calculo","Algebra","Fisica"),
                                names_to="ramos",values_to="resultado")

#Se realiza la prueba Q de Cochran usando la funcion cochran.qtest propia de R.
pruebacochran <- cochran.qtest(resultado ~ ramos | Id ,data = datos4, alpha = alfa)

#Se imprime el resultado de la prueba por consola.
print(pruebacochran)

#====================
#==== Respuesta 4 ===
#====================

#De la prueba anterior se obtiene un p-valor igual a 0.1182, el cual es mayor al nivel de significacion fijado,
#por lo que se falla al rechazar la hipotesis nula, concluyendo con un 95% de confianza que la proporcion de
#aprobacion es la misma para todos los ramos.

#Procedimiento post-hoc:
#Para este caso no fue necesario realizar un procedimiento post-hoc, ya que con la prueba Q de Cochran se determino
#que los 3 ramos tenian la misma proporcion de aprobacion. Y dado que el procedimiento post-hoc se utiliza para
#detectar cuales grupos son los que presentan una proporcion diferente, es innecesario realizarlo para estos
#datos.

#----------------------------------------------------------------------------------------
