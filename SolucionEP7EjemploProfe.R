
################################################################################
# Una de las primeras preguntas a responder por el �ltimo estudio nacional de
# obesidad infantil fue si exist�an diferencias en la prevalencia de la obesidad
# entre ni�os y ni�as o si, por el contrario, el porcentaje de obesos no var�a
# entre sexos. Se les solicita responder esta pregunta, contando con las
# primeras observaciones obtenidas en el estudio sobre una muestra de 14
# menores:

#       Obesidad
# Sexo	 S�	No
# Ni�a 	  1	 4
# Ni�o 	  7	 2
################################################################################

cat("\nPregunta 1\n\n")

# En este caso se tienen celdas de la tabla con menos de 5 observaciones, por lo
# que hay que usar una prueba para muestras peque�as.

# Como los datos son independientes (no pareados), corresponde usar la prueba
# exacta de Fisher, con las siguientes hip�tesis:
# H0: La obesidad infantil es independiente del sexo.
# HA: La obesidad infantil depende del sexo.

# Ahora podemos aplicar la prueba exacta de Fisher.
#Cargamos primero los datos
Ni�a <- c(1, 4)
Ni�o <- c(7, 2)
tabla.1 <- rbind(Ni�o, Ni�a)
colnames(tabla.1) <- c("S�", "No")
print(tabla.1)

# Establecemos un nivel de significaci�n.
alfa.1 <- 0.05

# Efectuamos la prueba exacta de Fisher puede hacerse m�s f�cilmente.

prueba.1 <- fisher.test(tabla.1, conf.level = 1 - alfa.1)
cat("\nResultado de la prueba exacta de Fisher:\n")
print(prueba.1)


# El valor p obtenido, 0,09091, es mayor que el nivel de significaci�n de
# 0,05, por lo que fallamos al rechazamos la hip�tesis nula. As�, podemos
# concluir, con 95% de confianza, que la proporci�n que la obesidad infantil no
# depende del sexo de los ni�os.

cat("=======================================================================\n")



################################################################################
# En un art�culo de Garc�a y colaboradores (2010) se describe un estudio en que
# se compararon diferentes versiones de algoritmos evolutivos para resolver
# variadas instancias de problemas de clasificaci�n tomadas desde el repositorio
# UCI Machine Learning. Suponga que la siguiente tabla muestra los resultados de
# la clasificaci�n hecha por dos versiones de un algoritmo gen�tico evaluado en
# el estudio para el problema Breast Cancer. �Consigue uno de los algoritmos
# mejor desempe�o?
#
#    AG v1	     AG v2
# Incorrecta	  Correcta
#   Correcta	  Correcta
# Incorrecta	  Correcta
#   Correcta	  Correcta
# Incorrecta	Incorrecta
# Incorrecta	  Correcta
#   Correcta	  Correcta
#   Correcta	Incorrecta
#   Correcta	Incorrecta
# Incorrecta	  Correcta
# Incorrecta	  Correcta
# Incorrecta	  Correcta
################################################################################

cat("\nPregunta 2\n\n")

# Primero construimos el data frame.
ag_v1 <- c("Incorrecta",	"Correcta",	"Incorrecta",	"Correcta",	"Incorrecta",
           "Incorrecta",	"Correcta",	"Correcta",	"Correcta",	"Incorrecta",
           "Incorrecta",	"Incorrecta")

ag_v2 <-	c("Correcta", "Correcta", "Correcta", "Correcta", "Incorrecta",
           "Correcta", "Correcta", "Incorrecta", "Incorrecta", "Correcta",
           "Correcta", "Correcta")

tabla.2 <- table(data.frame(ag_v1, ag_v2))
print(tabla.2)

# Como tenemos celdas con menos de 5 observaciones, se debe usar una prueba
# para muestras peque�as. Adem�s, sabemos que en este caso las observaciones
# est�n pareadas, por lo que corresponde usar la prueba de McNemar con las
# H0: Los algoritmos tienen igual desempe�o.
# HA: Los algoritmos tienen distinto desempe�o.

# Establecemos un nivel de significaci�n.
alfa.2 <- 0.05

# En R, tambi�n existe una funci�n para la prueba de McNemar.
cat("\nResultado de la prueba de McNemar:\n")
prueba.2 <- mcnemar.test(tabla.2)
print(prueba.2)

# El valor p obtenido, 0,2888, es mayor que el nivel de significaci�n de
# 0,05, por lo que fallamos al rechazamos la hip�tesis nula. As�, podemos
# concluir, con 95% de confianza, que no hay diferencia en el desempe�o de los
# algoritmos.

cat("=======================================================================\n")



################################################################################
# Una investigaci�n monitore� a m�s de 50 mil mujeres adultas durante 10 a�os
# (Lucas et al., 2011. Coffee, Caffeine, and Risk of Depression Among Women.
# Archives of Internal Medicine, 171(17), 1571-1578) con la intenci�n de
# identificar factores de riesgo de desarrollar un cuadro de depresi�n. Entre
# otras cosas, se registr� el consumo de cafe�na, cuyos datos se resumen en la
# siguiente tabla. �Existe alguna asociaci�n entre la cantidad de caf� que se
# bebe y la depresi�n?
#
#             <= 1 taza   2-6 tazas   1 taza   2-3 tazas   >= 4 tazas
# Depresi�n   semanal     semanales   al d�a   al d�a      al d�a        Total
#        S�         670         373      905         564           95    2.512
#        No      11.545       6.244   16.329      11.726        2.288   45.844
#     Total      12.215       6.617   17.234      12.290        2.383   48.356
################################################################################

cat("\nPregunta 3 (versi�n 1)\n\n")

# En este caso tenemos una tabla de contingencia de dos v�as y se busca
# determinar si existe relaci�n entre dos variables, por lo que una buena opci�n
# es usar una prueba chi-cuadrado de independencia. Las hip�tesis a contrastar
# son:
# H0: La incidencia de un cuadro depresivo en mujeres adultas no est� asociada
#     a la cantidad de caf� que estas consumen.
# HA: La cantidad de caf� consumido por una mujer adulta influye en su
#     posibilidad de desarrollar un cuadro depresivo.

# Comencemos por ingresar los datos.
con.depresion <- c(670, 373, 905, 564, 95)
sin.depresion <- c(11545, 6244, 16329, 11726, 2288)
tabla.3.1.obs <- rbind(con.depresion, sin.depresion)
cat("Frecuencias observadas:\n")
print(tabla.3.1.obs)

# Verifiquemos ahora las condiciones.
# Puesto que en cada categor�a son mujeres diferentes, y la muestra representa
# menos del 10% de la poblaci�n de mujeres que beben caf�, podemos asumir que
# las observaciones son independientes entre s�.

# Ahora debemos comprobar cu�ntas observaciones se esperan en cada grupo.
margen.fila <- apply(tabla.3.1.obs, 1, sum)
margen.columna <- apply(tabla.3.1.obs, 2, sum)
n.3.1 <- sum(tabla.3.1.obs)
tabla.3.1.esp <- margen.fila %*% t(margen.columna) / n.3.1
cat("\nFrecuencias esperadas:\n")
print(tabla.3.1.esp)

# Puesto que en cada caso se esperan m�s de 5 observaciones, podemos proceder
# sin problemas con la prueba seleccionada. Consideremos un nivel de
# significaci�n de 0,05.
prueba.3.1 <- chisq.test(x = tabla.3.1.obs)
cat("\nResultado de la prueba chi-cuadrado de independencia:\n")
print(prueba.3.1)

# El valor p obtenido es significativamente menor que el nivel de significaci�n
# fijado para la prueba, por lo que rechazamos la hip�tesis nula en favor de la
# hip�tesis alternativa. Concluimos entonces, con 95% de confianza, que la
# cantidad de caf� ingerida por una mujer influye en el desarrollo de un cuadro
# depresivo.

cat("=======================================================================\n")



################################################################################
# Un memorista, que est� trabajando con el grupo de investigaci�n de
# Aplicaciones para la Web de nuestro Departamento, ha dise�ado dos algoritmos
# de b�squeda que intentan considerar el estado de �nimo del usuario en los
# par�metros de la b�squeda. Por supuesto, necesita evaluar estas propuestas y
# para eso ha realizado un experimento computacional que mide las veces que el
# usuario necesita hacer solo una b�squeda para encontrar informaci�n relevante.
# La siguiente tabla muestra los resultados de estos experimentos, que tambi�n
# considera el algoritmo que actualmente utiliza uno de los motores de b�squeda
# m�s usados. �Existe alguna diferencia entre el rendimiento de los algoritmos
# probados?
#
#                     Actual   Nuevo 1   Nuevo 2    Total
#      Una b�squeda    3.511     1.749     1.798    7.058
# 2 o m�s b�squedas    1.489       751       702    2.942
#             Total    5.000     2.500     2.500   10.000
################################################################################

cat("\nPregunta 3 (versi�n 2)\n\n")

# Dado que tenemos una tabla de contingencia de dos v�as, una alternativa
# apropiada es la prueba chi-cuadrado. En este caso contamos con varias muestras
# (una para el algoritmo actual, otra para el algoritmo nuevo 1 y una �ltima
# para el algoritmo nuevo 2) para las cuales se midi� un �nico factor, que
# podr�amos llamar "eficiencia del algoritmo", con dos niveles:"una b�squeda" y
# "m�s de una b�squeda". As�, Como queremos ver si los algoritmos tienen
# desempe�os similares, se trara de una prueba chi-cuadrado de homogeneidad, con
# las siguientes hip�tesis a contrastar:
# H0: El desempe�o de todos los algoritmos es el mismo.
# HA: Al menos un algoritmo presenta un desempe�o disitnto al de los algoritmos
#     restantes.

# Comencemos por ingresar los datos.
una.b�squeda <- c(3511, 1749, 1818)
m�s.de.una.b�squeda <- c(1489, 751, 682)
tabla.3.2.obs <- rbind(una.b�squeda, m�s.de.una.b�squeda)
cat("Frecuencias observadas:\n")
print(tabla.3.2.obs)

# Verifiquemos ahora las condiciones.
# Puesto que en cada categor�a son mujeres diferentes, y la muestra representa
# menos del 10% de la poblaci�n de mujeres que beben caf�, podemos asumir que
# las observaciones son independientes entre s�.

# Ahora debemos comprobar cu�ntas observaciones se esperan en cada grupo.
margen.fila <- apply(tabla.3.2.obs, 1, sum)
margen.columna <- apply(tabla.3.2.obs, 2, sum)
n.3.2 <- sum(tabla.3.2.obs)
tabla.3.2.esp <- margen.fila %*% t(margen.columna) / n.3.2
cat("\nFrecuencias esperadas:\n")
print(tabla.3.2.esp)

# Puesto que en cada caso se esperan m�s de 5 observaciones, podemos proceder
# sin problemas con la prueba seleccionada. Consideremos un nivel de
# significaci�n de 0,05.
prueba.3.2 <- chisq.test(x = tabla.3.2.obs)
cat("\nResultado de la prueba chi-cuadrado de homogeneidad:\n")
print(prueba.3.2)

# El valor p obtenido es menor que el nivel de significaci�n fijado para la
# prueba, por lo que rechazamos la hip�tesis nula en favor de la hip�tesis
# alternativa. Concluimos entonces, con 95% de confianza, que a lo menos uno
# de los algoritmos de b�squeda presenta un desempe�o diferente al de los dem�s.

cat("=======================================================================\n")



################################################################################
# Estudios sobre las creencias de los estadounidenses acerca del origen y
# desarrollo de los seres humanos se llevan haciendo desde hace d�cadas. En la
# �ltima encuesta, realizada en 2010, se presentaron las siguientes opciones:
# a) Human beings have developed over millions of years from less advanced forms
#    of life, but God guided this process.
# b) Human beings have developed over millions of years from less advanced forms
#    of life, but God had no part in this process.
# c) God created human beings pretty much in their present form at one time
#    within the last 10,000 years or so.
# 1.019 personas fueron consultadas sobre cu�l de las opciones anteriores
# representaba mejor su punto de vista. 387 personas se inclinaron por la opci�n
# 1, 171 por la opci�n 2, 400 por la opci�n 3 y 61 no supieron o no quisieron
# responder. En el a�o 2007, esta misma encuesta registr� las siguientes
# proporciones: 38% opci�n 1, 14% opci�n 2, 43% opci�n 3. �Han cambiado las
# creencias de los estadounidenses acerca del origen y desarrollo de los seres
# humanos desde 2007?
################################################################################

cat("\nPregunta 3 (versi�n 3)\n\n")

# Este problema tambi�n puede enfrentarse con un procedimiento chi-cuadrado,
# que usualmente se conoce como prueba de bondad de ajuste (goodness-of-fit en
# ingl�s), donde se tabulan las frecuencias observadas de una variable en
# categor�as y luego se comparan con las frecuencias esperadas en cada una de
# dichas categor�as de acuerdo a una distribuci�n conocida de referencia. Las
# hip�tesis a contrastar son:
# H0: Los estadounidenses han mantenido sus creencias, entre 2007 y 2010, con
#     respecto al origen y desarrollo de los seres humanos.
# HA: Los estadounidenses han modificado sus creencias, entre 2007 y 2010, con
#     respecto al origen y desarrollo de los seres humanos.

# En este caso, tenemos frecuencias observadas el 2010 y conocemos la
# distribuci�n que ten�a la "poblaci�n" en 2007, lo que usaremos como
# referencia para la comparaci�n.

frecuencia.2010 <- c(387, 171, 400)
proporcion.2007 <- c(38, 14, 43) / 100

# En base a estos datos, podemos calcular las frecuencias de 2007 si el tama�o
# de la muestra hubiera sido el mismo que en 2010.
n.3.3 <- sum(frecuencia.2010) + 61
frecuencia.2007 <- proporcion.2007 * n.3.3

# As�, la tabla de datos para este problema es:
tabla.3.3.obs <- rbind(frecuencia.2010, frecuencia.2007)
cat("Frecuencias observadas:\n")
print(tabla.3.3.obs)

# Ahora verificamos las condiciones.
# Puesto que se encuest� a menos del 10% de la poblaci�n de Estados Unidos,
# podemos afirmar que las observaciones son independientes entre s�.

# Ahora debemos calcular la cantidad de observaciones esperadas por cada grupo.
esperadas <- proporcion.2007 * n.3.3
cat("\nFrecuencias esperadas para 2010:\n")

# Puesto que en cada caso se esperan m�s de 5 observaciones, es pertinente
# proceder con la prueba. Consideremos un nivel de significaci�n de 0,05.
prueba.3.3 <- chisq.test(x = tabla.3.3.obs)
cat("\nResultado de la prueba chi-cuadrado de bondad de ajuste:\n")
print(prueba.3.3)

# El valor p obtenido es mayor que el nivel de significaci�n fijado para la
# prueba, por lo que fallamos al rechazar la hip�tesis nula. Concluimos pues,
# con 95% de confianza, que no ha habido un cambio en las creencias de los
# estadounidenses acera del origen y desrrollo de los seres humanos.

cat("=======================================================================\n")



################################################################################
# La Facultad de Ingenier�a desea saber si existe diferencia significativa en el
# desempe�o de los estudiantes en asignaturas cr�ticas de primer semestre. Para
# ello, le ha entregado un archivo de datos que, para 3 asignaturas, indica si
# una muestra de 50 estudiantes aprob� o reprob�. �Qu� puede concluir la
# Facultad? Indicaci�n: obtenga la muestra a partir del archivo EP07 Datos.csv,
# usando la semilla 600. Considere un nivel de significaci�n ??=0,05.
################################################################################

cat("\nPregunta 4\n\n")

# En esta pregunta tenemos una variable independiente (el estudiante) que tiene
# 3 observaciones pareadas de una variable de respuesta dicot�mica (si aprueba o
# reprueba cada una de las asignaturas). Una herramienta que conocemos para
# este escenario es la prueba Q de Cochran, con las siguientes hip�tesis:
# H0: La tasa de aprobaci�n es la misma para C�lculo, �lgebra y F�sica.
# HA: Al menos una de las asignaturas (C�lculo, �lgebra y F�sica) tiene una tasa
#     de aprobaci�n distinta.

# Cargamos los datos.
setwd("D:/Dropbox/Inferencia/Ejercicios pr�cticos 1-2022/EP07")
datos <- read.csv2(file = "EP07 Datos.csv", stringsAsFactors = TRUE)

# Obtenemos la muestra.
library(dplyr)
set.seed(600)
muestra <- sample_n(datos, size = 50, replace = FALSE)

# Como siempre, comenzamos por verificar las condiciones.
# La variable de respuesta es dicot�mica con niveles A (aprueba) y R (reprueba) y
# la variable independiente, correspondiente a la asignatura, es categ�rica con
# los niveles C�lculo, �lgebra y F�sica.
# Puesto que la muestra de estudiantes es seleccionada al azar y que el tama�o de
# la muestra (50) corresponde a menos del 10% de los estudiantes que
# hist�ricamente han cursado la asignatura, podemos asumir que las observaciones
# son independientes entre s�.
# Por �ltimo, nuestra muestra tiene 50 observaciones y 3 niveles en la variable
# independiente, por lo que se cumple que 50 * 3 = 150 >= 24.
# En consecuencia, podemos usar la prueba Q de Cochran para este problema.


# Llevamos los datos a formato largo, seg�n requiere la prueba.
library(tidyverse)

muestra <- muestra %>% pivot_longer(c("Calculo", "Algebra", "Fisica"),
                                    names_to = "Curso", values_to = "Situacion")

muestra[["Curso"]] <- factor(muestra[["Curso"]])

# Hacemos ahora la prueba Q de Cochran. Recordemos que, como esta prueba
# simplemente comprueba la igualdad de todas las proporciones, se trata de una
# prueba �mnibus.

library(RVAideMemoire)
alfa <- 0.05
prueba.4 <- cochran.qtest(Situacion ~ Curso | Id, data = muestra, alpha = alfa)
cat("Resultado de la prueba Q de Cochran\n")
print(prueba.4)

# El valor p obtenido es menor que el nivel de significaci�n fijado para la
# prueba, por lo que rechazamos la hip�tesis nula en favor de la hip�tesis
# alternativa. Concluimos pues, con 95% de confianza, que a lo menos una de las
# asignaturas (C�lculo, �lgebra o F�sica) tiene una tasa de aprobaci�n
# distinta a las dem�s.

# Como la prueba �mnibus encontr� diferencias estad�sticamente significativas,
# debemos ahora llevar a cabo un procedimiento post-hoc para determinar
# cu�les son esas diferencias. Puesto que tiene un mayor poder estad�stico,
# usaremos la correcci�n de Holm.
library(rcompanion)

post.hoc <- pairwiseMcnemar(Situacion ~ Curso | Id, data = muestra,
                            method = "holm")

cat("\nResultado del procedimiento post-hoc usando la correcci�n de Holm")
print(post.hoc)

# El resultado obtenido nos muestra que existe una diferencia significativa
# entre �lgebra y C�lculo, aunque no se evidencia una diferencia importante
# entre ninguna de estas dos asignaturas y F�sica.