library(Hmisc)

#====================
#==== Pregunta 1 ====
#====================

#Estudios previos habian determinado que la proporcion de autoras en la especialidad de 
#neurologia era de 32%. ¿respaldan estos datos tal estimacion?

#Hipotesis:

#H0: la proporcion de autoras en la especialidad de neurologia es igual a 32%.
#HA: la proporcion de autoras en la especialidad de neurologia es distinta de 32%.

#Formula matematica:

#H0: p = 0.32
#HA: p != 0.32

#Numero total de autores en la especialidad de neurologia.
total_neuro <- 144
#Numero de autoras en la especialidad de neurologia.
total_mujeres <- 56

#Se calcula la probabilidad de exito (probabilidad de que sea una autora).
p_exito <- total_mujeres/total_neuro
#Propabilidad nula dada por el enunciado.
p_nulo <- 0.32

#Nivel de significacion fiajdo a 0.05.
alpha <- 0.05
#Se calcula la cantidad de exitos multiplicando la probabilidad de exito por la cantidad total
#de autores de neurologia.
cant_exitos <- p_exito * total_neuro

#Para determinar cual de las hipotesis es la correcta se realiza una prueba de proporcion
#usando el metodo de Wilson, con la funcion prop.test.
prueba1 <- prop.test(cant_exitos,n = total_neuro, p = p_nulo, alternative="two.sided", conf.level = 1 - alpha)

#Se imprime el resultado de la prueba anterior.
print(prueba1)

#====================
#==== Respuesta 1 ===
#====================

#En base a los datos entregados, se tiene p-valor = 0.09 > alpha
# por lo que se falla en rechazar H0 en favor de HA, por lo que se concluye con 95% de confianza,
# que la proporcion de autoras en la especialidad de neurologia es distinta de 32%.
#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 2 ====
#====================

#Segun estos datos, ¿es igual la proporcion de autoras en las areas de anestesiologia y obstetricia?

#Hipotesis:

#H0: la proporcion de autoras en la especialidad de anestesiologia es igual a la proporcion de autoras en
#    la especialidad de obstetricia.
#HA: la proporcion de autoras en la especialidad de anestesiologia es distinta a la proporcion de autoras en
#    la especialidad de obstetricia.

#Formula matematica:

#pA = proporcion de autoras en anestesiologia.
#pO = proporcion de autoras en obstetricia.
#H0: pA = pO
#HA: pA != pO

#Muestra total de autores (hombres y mujeres) en anestesiologia.
total_anest <- 61
#Muestra total de autores (hombres y mujeres) en obstetricia
total_obt <- 137

#Cantidad de autoras en obstetricia.
total_mujeres_obt <- 71
#Cantidad de autoras en anestesiologia.
total_mujeres_anest <- 21

#Se fijan los valores conocidos
#Cantidades totales de autores en anestesiologia y obstetricia.
n <- c(c(total_anest, total_obt))
#Cantidades de exito en anestesiologia y obstetricia.
exitos <- c(total_mujeres_anest, total_mujeres_obt)
#Se fija el nivel de significacion en 0.05
alpha <- 0.05
#Se fija el valor nulo en 0.0
p_nulo <- 0.0

#Se utiliza el metodo de Wilson usando la funcion prop.test.
prueba2 <- prop.test(exitos, n = n, alternative = "two.sided", conf.level = 1 - alpha)
#Se imprimen los resultados de la prueba.
print(prueba2)

#====================
#==== Respuesta 2 ===
#====================

#En este caso se obtiene un p-valor = 0.034 < alpha fijado, por lo que se rechaza H0 en favor de HA.
# por lo que los datos sugieren que si existen proporciones de autoras en la especialidad
# anestesiologia distintas a las autoras de la especialidad de obstetricia.

#----------------------------------------------------------------------------------------

#====================
#==== Pregunta 3 ====
#====================

# suponiendo que la diferencia en la proporcion de autoras en la especialidad de psiquiatria y la de medicina 
# interna es de 0,23. ¿a cuantos autores deberiamos monitorear para obtener un intervalo de confianza del 97,5% 
# y poder estadistico de 90%, si se intenta mantener aproximadamente la misma proporcion de gente estudiada en cada caso?

#n1 = autoras de Psiquiatria
#n2 = autoras de Medicina interna
n1 = 40
n2 = 35
#Definimos las proporciones
prop1 <- n1/72
prop2 <- n2/110
#Debido a que la diferencia propuesta es de 0.23
#se ajusto las poblaciones para acercarse a esta diferencia
diferencia <- prop1 - prop2
diferencia <- abs(diferencia)
#Se define el poder requerido y el intervalo de confianza
poder3 <- 0.9
alpha3 <- 0.025
#Se define la fraccion correspondiente de la funcion, en este caso sobre n1 que
#que corresponde a las autoras de Psiquiatria
fraction <- n1/(n1+n2)

#Finalmente se obtiene el numero de personas a monitorear
tam <- bsamsize(prop1,prop2,fraction, alpha3,poder3)


#====================
#==== Respuesta 3 ===
#====================
#La respuesta es entrega por consola
print(tam)

cat("Autores a monitorear en Psiquiatria: ",ceiling(tam[1][1]),"\n",
    "Autores a monitorear en Medicina Interna: ",ceiling(tam[2][1]),"\n")

#----------------------------------------------------------------------------------------
