
library(Hmisc)

# Ingresamos los datos.
datos <- data.frame(
  Especialidad = factor(c("Pediatr�a", "Obstetricia", "Dermatolog�a",
                          "Psiquiatr�a", "M. Interna", "Oncolog�a",
                          "Neurolog�a", "Anestesiolog�a", "Radiolog�a")),
  Mujeres = c(54, 71, 35, 30, 45, 44, 56, 21, 17),
  Hombres = c(52, 66, 41, 42, 65, 62, 88, 40, 35)
)



################################################################################
# Estudios previos hab�an determinado que la proporci�n de autoras en la
# especialidad de psiquiatr�a era de 27%. �Respaldan estos datos tal estimaci�n?
################################################################################

cat("\nPregunta 1\n")

# La pregunta corresponde a una inferencia de una proporci�n con una muestra, la
# que podemos enfrentar con el m�todo de Wilson para una proporci�n.

# Comencemos por formular las hip�tesis.
# H0: La proporci�n de autoras en el �rea de psiquiatr�a es 27% (p = 0,27).
# HA: La proporci�n de autoras en el �rea de psiquiatr�a no es 27% (p != 0,27).

# Definimos los valores conocidos.
valor.nulo <- 0.27
mujeres <- datos[["Mujeres"]][4]
hombres <- datos[["Hombres"]][4]
n <- mujeres + hombres

# Ahora verifiquemos las condiciones.
# No nos queda m�s que confiar en que los responsables del estudio fueron
# cuidadosos a este respecto y las observaciones son independientes entre s�.

# La condici�n de �xito fracaso establece que se espera observar al menos 10
# observaciones correspondientes a �xito (mujeres, en este caso) y al menos 10,
# correspondientes a fracasos (hombres).
exitos.esperados <- n * valor.nulo
fracasos.esperados <- n * (1 - valor.nulo)

cat("\nSe esperan", exitos.esperados, "mujeres y", fracasos.esperados,
    "hombres\n")

# Podemos ver que las cantidades esperadas de �xitos y fracasos superan con
# creces el l�mite inferior de 10.

# Una vez verificadas las condiciones, procedemos con la prueba. No tenemos
# motivos para ser m�s exigentes, por lo que fijaremos un nivel de significaci�n
# (alfa) de 0,05.
prueba.1 <- prop.test(x = mujeres, n = n, p = valor.nulo,
                      alternative = "two.sided", conf.level = 0.95,
                      correct = FALSE)

print(prueba.1)

# El valor p obtenido, 0,00506, es mucho menor que el nivel de significaci�n de
# 0,05, por lo que rechazamos la hip�tesis nula en favor de la hip�tesis
# alternativa. As�, podemos concluir, con 95% de confianza, que la proporci�n
# de autoras que publican art�culos en el �rea de psiquiatr�a difiera de 27%.

cat("=======================================================================\n")



################################################################################
# Seg�n estos datos, �es igual la proporci�n de autoras en las �reas de
# psiquiatr�a y neurolog�a?
################################################################################

cat("\nPregunta 2\n")

# Esta nueva pregunta corresponde a una inferencia acerca de la diferencia entre
# dos proporciones. Una vez m�s podemos usar el m�todo de Wilson, pero ahora
# para la diferencia entre dos proporciones.

# Comencemos por formular las hip�tesis.
# H0: La proporci�n de autoras es la misma en las �reas de psiquiatr�a y
#     neurolog�a (p1 - p2 = 0).
# HA: La proporci�n de autoras es distinta en las �reas de psiquiatr�a y
#     neurolog�a (p1 - p2 != 0).

# Definimos los valores conocidos.
mujeres.psiquiatria <- datos[["Mujeres"]][4]
hombres.psiquiatria <- datos[["Hombres"]][4]
mujeres.neurologia <- datos[["Mujeres"]][7]
hombres.neurologia <- datos[["Hombres"]][7]
n.psiquiatria <- mujeres.psiquiatria + hombres.psiquiatria
n.neurologia <- mujeres.neurologia + hombres.neurologia

# Ahora verifiquemos las condiciones.
# No tenemos mayor informaci�n acerca de c�mo se obtuvieron las muestras, pero
# podemos suponer que los autores del estudio inicial fueron rigurosos y que se
# cumple la condici�n de independencia.

# La condici�n de �xito fracaso establece que se espera observar al menos 10
# observaciones correspondientes a �xito (mujeres, en este caso) y al menos 10,
# correspondientes a fracasos (hombres) en cada una de las muestras, lo que
# podemos verificar f�cilmente en los datos: en el caso de psiquiatr�a,
# tenemos 30 y 42 observaciones, y en neurolog�a, 56 y 88.

# Una vez verificadas las condiciones, procedemos con la prueba. No tenemos
# motivos para ser m�s exigentes, por lo que fijaremos un nivel de significaci�n
# (alfa) de 0,05.

prueba.2 <- prop.test(x = c(mujeres.psiquiatria, mujeres.neurologia),
                      n = c(n.psiquiatria, n.neurologia),
                      alternative = "two.sided", conf.level = 0.95,
                      correct = FALSE)

print(prueba.2)

# El valor p obtenido, 0,6942, es mucho mayor que el nivel de significaci�n de
# 0,05, por lo que fallamos al rechazar la hip�tesis nula. Concluimos entonces,
# con 95% de confianza, que la proporci�n de autoras que publican art�culos en
# las �reas de psiquiatr�a y neurolog�a es la misma.

cat("=======================================================================\n")



################################################################################
# Suponiendo que la diferencia en la proporci�n de autoras la especialidad de
# radiolog�a y la de oncolog�a es de 0,17. �A cu�ntos autores deber�amos
# monitorear para obtener un intervalo de confianza del 97,5% y poder
# estad�stico de 85%, si se intenta mantener aproximadamente la misma proporci�n
# de gente estudiada en cada caso?
###############################################################################

# Esta pregunta nuevamente aborda la diferencia de proporciones entre dos
# grupos, pero ahora se solicita el tama�o de la muestra.

# Las hip�tesis asociadas a esta pregunta son, (considerando que no nos dan
# informaci�n acerca de la direcci�n de la hip�tesis alternativa):
# H0: Hay una diferencia de 17% en la proporci�n de autoras en las �reas de
#     radiolog�a y oncolog�a (|p1 - p2| = 0,17).
# HA: No hay una diferencia de 17% en la proporci�n de autoras en las �reas de
#     radiolog�a y oncolog�a (|p1 - p2| != 0,17).

# Veamos las proporciones observadas:
n.observado.radiologia <- datos[["Mujeres"]][9] + datos[["Hombres"]][9]
p.observada.mujeres.radiologia <- datos[["Mujeres"]][9] / n.observado.radiologia

n.observado.oncologia <- datos[["Mujeres"]][6] + datos[["Hombres"]][6]
p.observada.mujeres.oncologia <- datos[["Mujeres"]][6] / n.observado.oncologia

cat("\n")
cat("Casos observados en radiolog�a:", n.observado.radiologia, "\n")

cat("Proporci�n observada de mujeres en radiolog�a:",
    p.observada.mujeres.radiologia, "\n")

cat("\n")
cat("Casos observados en oncolog�a:", n.observado.oncologia, "\n")

cat("Proporci�n observada de mujeres en oncolog�a:",
    p.observada.mujeres.oncologia, "\n")

# Ahora intentemos definir proporciones "esperadas" que se acerquen 
# a las observadas, pero con la diferencia establecida en la hip�tesis.
p.esperada.mujeres.radiologia <- .28
p.esperada.mujeres.oncologia <- .45
diferencia <- p.esperada.mujeres.radiologia - p.esperada.mujeres.oncologia

# Ahora calculamos el tama�o de las muestras, que evidentemente tienen tama�os
# diferentes.
alfa <- 0.025
poder <- 0.85
fraccion <- n.observado.radiologia / (n.observado.radiologia + n.observado.oncologia)

resultado <- bsamsize(p1 = p.esperada.mujeres.radiologia,
                      p2 = p.esperada.mujeres.oncologia,
                      fraction = fraccion,
                      alpha = alfa, power = poder)

cat("\nResultado de la funci�n bsamsize():\n\n")
print(resultado)

# Calculamos ahora los tama�os de las muestras (que, obviamente, deben ser
# n�meros enteros positivos).
n.radiologia <- ceiling(resultado[1])
n.oncologia <- ceiling(resultado[2])

cat("\nSe deben considerar", n.radiologia, "autores en radiolog�a y",
    n.oncologia, "en oncolog�a")