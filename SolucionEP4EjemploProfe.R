# Importar paquetes
library(ggpubr)
library(dplyr)

# Indicar directorio.
setwd("D:/Dropbox/Inferencia/Ejercicios pr�cticos 1-2022/EP04")

# Cargar datos.
datos <- read.csv2("EP04 datos.csv")

# El Comit� Ol�mpico de una gran potencia del atletismo est� estudiando el
# programa de entrenamiento de varones para la competencia de 100 metros planos,
# por lo que ha recopilado datos de diversos atletas:
# -	Id: identificador �nico para cada atleta.
# -	Raza: raza del atleta (categ�rica: Blanca, Negra, Oriental).
# -	Previo: mejor tiempo registrado por el atleta antes de ingresar al programa
#   de entrenamiento (num�rica, en segundos).
# -	Posterior: mejor tiempo registrado por el atleta durante los primeros 6
#   meses del programa de entrenamiento (num�rica, en segundos).



################################################################################
# El Comit� Ol�mpico cree que el mejor tiempo medio de los atletas blancos antes
# de ingresar al programa de entrenamiento es de 17,23 segundos. �Soportan los
# datos esta afirmaci�n?
################################################################################

cat("Pregunta 1\n")

# En este caso, debemos inferir acerca de la media de una muestra. Veamos el
# tama�o de la muestra.
muestra.1 <- datos[datos[["Raza"]] == "Blanca",][["Previo"]]
n.1 <- length(muestra.1)
cat("Tama�o de la muestra:", n.1, "\n")

# Como la muestra es peque�a (menos de 30 observaciones), ser�a adecuado usar
# la prueba t de Student para una muestra. Pero antes debemos verificar las
# condiciones.

# Como se trata de atletas diferentes y la muestra representa menos del 10% de
# la poblaci�n, podemos asumir que las observaciones son independientes entre
# s�.

# Ahora debemos verificar si las observaciones presentan una distribuci�n
# cercana a la normal. Una forma de hacer esto es mediante la prueba de
# normalidad de Shapiro-Wilk.
normalidad.1 <- shapiro.test(muestra.1)
cat("\nComprobaci�n de la normalidad de los datos:\n")
print(normalidad.1)

# Puesto que el valor p obtenido es muy alto, podemos suponer razonablemente
# que esta condici�n se verifica, por lo que podemos aplicar la prueba
# seleccionada.

# Como no hay indicios de que tengamos que ser cautelosos con los resultados,
# fijamos el nivel de significaci�n en 0,05.
alfa.1 <- 0.05

# Ahora debemos formular las hip�tesis:
# H0: Antes del entrenamiento, la media de las mejores marcas de los atletas
#     blancos en 100 metros planos es de 17,23 segundos (mu = 17,23 [s]).
# HA: Antes del entrenamiento, la media de las mejores marcas de los atletas
#     blancos en 100 metros planos es distinta de 17,23 segundos
#     (mu != 17,23 [s]).

# Efectuamos la prueba:
valor.nulo.1 <- 17.23

prueba.1 <- t.test(muestra.1, alternative = "two.sided", mu = valor.nulo.1,
                   conf.level = 1-alfa.1)

cat("Prueba de hip�tesis:\n")
print(prueba.1)

# El valor p obtenido es menor que el nivel de significaci�n (0,001255 < 0,05),
# por lo que rechazamos la hip�tesis nula en favor de la hip�tesis alternativa.

# En consecuencia, podemos concluir con 95% de confianza que, en promedio, la
# mejor marca de los atletas blancos en los 100 metros planos antes del
# entrenamiento no es de 17,23 segundos.

cat("=======================================================================\n")



################################################################################
# �Sugieren los datos que la mejor marca de los atletas orientales se reduce en
# menos de 6,45 segundos tras el entrenamiento?
################################################################################

cat("\nPregunta 2\n")

# En este caso, debemos inferir acerca de la media de dos muestras pareadas
# (media de las diferencias).

# Obtengamos las muestras con las que trabajaremos.
anterior <- datos[datos[["Raza"]] == "Oriental",][["Previo"]]
posterior <- datos[datos[["Raza"]] == "Oriental",][["Posterior"]]

# Veamos ahora el tama�o de las muestras (ambas tienen el mismo, al estar
# pareadas).
n.2 <- length(anterior)
cat("Tama�o de la muestra:", n.2, "\n")

# Como la muestra es peque�a (menos de 30 observaciones), ser�a adecuado usar
# la prueba t de Student para dos muestras pareadas. Pero antes debemos
# verificar las condiciones.

# Como se trata de atletas diferentes, menor al 10% de la poblaci�n, podemos
# suponer que las observaciones son independientes entre s�.

# Ahora debemos verificar si las diferencias presentan una distribuci�n
# cercana a la normal. Una forma de hacer esto es mediante un gr�fico Q-Q.
diferencias <- anterior - posterior

g2 <- ggqqplot(data.frame(diferencias), x = "diferencias", color = "blue")
print(g2)

# La forma de los datos en el gr�fico no se aleja tanto de una recta, y podemos
# ver que no hay evidencia de valores at�picos, pues no hay puntos fuera de la
# banda coloreada.

# Como no hay indicios de que tengamos que ser cautelosos con los resultados,
# fijamos el nivel de significaci�n en 0,05.
alfa.2 <- 0.05

# Ahora debemos formular las hip�tesis:
# H0: Tras el entrenamiento, la media de las mejores marcas de los atletas
#     orientales en los 100 metros planos se reduce en 6,45 segundos
#     (mu.antes-despu�s = 6,45 [s]).
# HA: Tras el entrenamiento, la media de las mejores marcas de los atletas
#     orientales en los 100 metros planos se reduce en menos de 6,45 segundos
#     (mu.antes-despu�s < 6,45 [s]).

# Efectuamos la prueba:
valor.nulo.2 <- 6.45

prueba.2 <- t.test(x = anterior, y = posterior, alternative = "less",
                   mu = valor.nulo.2, paired = TRUE, conf.level = 1-alfa.2)

cat("Prueba de hip�tesis:\n")
print(prueba.2)

# El valor p obtenido es mayor que el nivel de significaci�n (0,2098 < 0,05),
# por lo que fallamos en rechazar la hip�tesis nula.

# En consecuencia, podemos concluir con 95% de confianza que, en promedio, la
# mejor marca de los atletas orientales en los 100 metros planos se reduce en
# 6,45 segundos tras el entrenamiento.

cat("=======================================================================\n")



################################################################################
# �Es posible afirmar que, en promedio, los atletas negros superan a los blancos
# por m�s de XX segundos despu�s del entrenamiento?
################################################################################

cat("\nPregunta 3\n")

# En este caso, debemos inferir acerca de la diferencia entre las medias de dos
# muestras independientes (diferencia de las medias).

# Obtengamos las muestras con las que trabajaremos.
negros <- datos[datos[["Raza"]] == "Negra",][["Posterior"]]
blancos <- datos[datos[["Raza"]] == "Blanca",][["Posterior"]]

# Veamos ahora el tama�o de las muestras.
n.3.negros <- length(negros)
n.3.blancos <- length(blancos)
cat("Tama�o de las muestras:", n.3.negros, "y", n.3.blancos, "\n")

# Como las muestras son peque�as (menos de 30 observaciones), ser�a adecuado
# usar la prueba t de Student para dos muestras independientes. Pero antes
# debemos verificar las condiciones.

# Como en el caso de ambas muestras se trata de atletas diferentes, menor al
# 10% de la poblaci�n respectiva, podemos suponer que las observaciones son
# independientes entre s�.

# Ahora debemos verificar si cada una de las muestras presenta una distribuci�n
# cercana a la normal.
cat("\nComprobaci�n de la normalidad de los datos:\n")
normalidad.3.negros <- shapiro.test(negros)
normalidad.3.blancos <- shapiro.test(blancos)
cat("Primera muestra:\n")
print(normalidad.3.negros)
cat("Segunda muestra:\n")
print(normalidad.3.blancos)

# Podemos ver que, en ambos casos, el valor p que se obtiene es bastante alto,
# por lo que es razonable suponer que ambas muestas provienen de una
# distribuci�n cercana a la normal.

# Como no hay indicios de que tengamos que ser cautelosos con los resultados,
# fijamos el nivel de significaci�n en 0,05.
alfa.3 <- 0.05

# Ahora debemos formular las hip�tesis:
# �Es posible afirmar que, en promedio, los atletas negros superan a los blancos
# por m�s de XX segundos despu�s del entrenamiento?
# H0: Tras el entrenamiento, los atletas negros superan a los blancos en la
#     carrera de 100 metros planos por, en promedio, XX segundos
#     (media.blancos - media.negros = XX [s]).
# HA: Tras el entrenamiento, los atletas negros superan a los blancos en la
#     carrera de 100 metros planos, en promedio, por m�s de XX segundos
#     (media.blancos - media.negros > XX [s]).

# Efectuamos la prueba:
valor.nulo.3 <- 2.03

prueba.3 <- t.test(x = blancos, y = negros, alternative = "greater",
                   mu = valor.nulo.3, paired = FALSE, conf.level = 1-alfa.3)

cat("Prueba de hip�tesis:\n")
print(prueba.3)

# El valor p obtenido es mayor que el nivel de significaci�n (0,2098 < 0,05),
# por lo que fallamos en rechazar la hip�tesis nula.

# En consecuencia, podemos concluir con 95% de confianza que, en promedio, la
# mejor marca de los atletas orientales se reduce en 6,45 segundos tras el
# entrenamiento.
