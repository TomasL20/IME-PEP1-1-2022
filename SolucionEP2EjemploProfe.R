# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios pr�cticos 1-2022/EP02")

# Fijar una semilla para asegurar que los resultados sean reproducibles.
set.seed(87)

# Importar paquetes.
library(dplyr)
library(ggpubr)

# Cargar datos.
datos <- read.csv2("EP02 Datos Casen 2017.csv", stringsAsFactors = TRUE)

# Obtener una muestra de 150 observaciones.
n_muestra <- 150
muestra <- sample_n(datos, n_muestra)

# Si reescalamos la variable ytot de $1 a $1.000, tendremos n�meros m�s claros.
datos[["ytot"]] <- datos[["ytot"]] / 1000
muestra[["ytot"]] <- muestra[["ytot"]] / 1000



################################################################################
# AN�LISIS DE FRECUENCIAS
# �Se encuest� m�s o menos la misma cantidad de gente en cada provincia de
# la RM?
################################################################################

cat("�Se encuest� m�s o menos la misma cantidad de gente en cada provincia")
cat("de la RM?\n")

# Comencemos con la muestra.
# Contar cu�ntas observaciones hay por cada provincia.
freq_m <- as.data.frame(table(muestra[["provincia"]]))
colnames(freq_m) <- c("Provincia", "Frecuencia")
cat("\nEncuestados por provincia para la muestra\n")
print(freq_m)

# Construir gr�fico de barras.
g1_1_m <- ggbarplot(freq_m, x = "Provincia", y = "Frecuencia",
                    label = TRUE, lab.pos = "out", lab.col = "black",
                    fill = "Provincia", palette = "jco",
                    title = "Encuestados en la RM por provincia",
                    subtitle = "Muestra")

g1_1_m <- g1_1_m + rotate_x_text(angle = 45)
print(g1_1_m)

# Otra opci�n puede ser un gr�fico de torta.
g1_2_m <- ggpie(freq_m, x = "Frecuencia", label = "Frecuencia",
                lab.pos = "out", lab.col = "black", fill = "Provincia",
                color = "black", palette = "jco",
                title = "Encuestados en la RM por provincia")

print(g1_2_m)

# Los gr�ficos obtenidos muestran que la cantidad de encuestados no es la misma
# en cada provincia. De hecho, la cantidad de encuestados en la provincia de
# Santiago parece ser significativamente mayor. Si miramos las frecuencias,
# Santiago tuvo la mator cantidad de encuestados (116), seguida de Talagante
# �con apenas 12!

# La frecuencia es una medida �til para responder esta pregunta. Sin embargo,
# suele estudiarse la proporci�n, que corresponde a un estimador de la
# probabilidad.
proporciones_m <- freq_m[["Frecuencia"]] / n_muestra
i <- freq_m[["Provincia"]] == "Santiago"

prop_m <- data.frame(Provincia = c("Stgo.", "Otra"),
                     Proporcion = c(proporciones_m[i],
                                    sum(proporciones_m[!i]))
)

cat("\nProporci�n de encuestados en la provincia de Stgo. para la muestra\n")
print(prop_m)

# Las proporciones evidencian que m�s de 75% de los encuestados de la muestra
# son de la provincia de Santiago.

# Ahora veamos qu� pasa con la poblaci�n.
# Contar cu�ntas observaciones hay por cada provincia.
freq_p <- as.data.frame(table(datos[["provincia"]]))
colnames(freq_p) <- c("Provincia", "Frecuencia")
cat("\nEncuestados por provincia para la poblaci�n\n")
print(freq_p)

# Construir gr�fico de barras.
g1_1_p <- ggbarplot(freq_p, x = "Provincia", y = "Frecuencia",
                    label = TRUE, lab.pos = "out", lab.col = "black",
                    fill = "Provincia", palette = "jco",
                    title = "Encuestados en la RM por provincia",
                    subtitle = "Poblaci�n")

g1_1_p <- g1_1_p + rotate_x_text(angle = 45)
print(g1_1_p)

# Calcular las proporciones.
proporciones_p <- freq_p[["Frecuencia"]] / nrow(datos)
i <- freq_p[["Provincia"]] == "Santiago"

prop_p <- data.frame(Provincia = c("Stgo.", "Otra"),
                     Proporcion = c(proporciones_p[i],
                                    sum(proporciones_p[!i]))
)

cat("\nProporci�n de encuestados en la provincia de Stgo. para la poblaci�n\n")
print(prop_p)

# Crear figura con los gr�ficos de la muestra y de la poblaci�n.
g1 <- ggarrange(g1_1_m, g1_1_p, ncol = 2, nrow = 1)
print(g1)

# Podemos ver que la muestra refleja de manera bastante cercana lo que ocurre en
# la poblaci�n. Se mantiene que m�s de 75% de los encuestados son de la
# provincia de Santiago, aunque la proporci�n es ligeramente menor.

cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# AN�LISIS DE UNA VARIABLE NUM�RICA
# �C�mo podemos describir el ingreso de los chilenos de la RM?
################################################################################

cat("\n�C�mo podemos describir el ingreso de los chilenos de la RM?\n")

# Como el ingreso se acerca bastante a ser una variable continua, podemos usar
# varios tipos diferentes de gr�ficos para explorarla.

# Comencemos con la muestra.

# Una buena alternativa puede ser un histograma.
g2_1_m <- gghistogram(muestra, x = "ytot", bins = 15, add = "mean",
                      rug = TRUE, color = "#6D9EC1", fill = "#BFD5E3",
                      title = "Ingreso en la RM", subtitle = "Muestra",
                      xlab = "Ingreso total (miles de pesos)", ylab = "Frecuencia")

g2_1_m <- ggpar(g2_1_m, x_lim = c(0, 5050))
print(g2_1_m)

# Otra opci�n m�s detallada puede ser un diagrama de puntos, aunque para ello
# tendremos que usar funciones m�s b�sicas del paquete ggplot2.
g2_2_m <- ggplot(muestra, aes(x = ytot))

g2_2_m <- g2_2_m + geom_dotplot(binwidth = 100, color = "#6D9EC1",
                                fill = "#BFD5E3")

g2_2_m <- g2_2_m + xlim(0, 5050)
g2_2_m <- g2_2_m + ggtitle("Ingreso en la RM", subtitle = "Muestra")
g2_2_m <- g2_2_m + xlab("Ingreso total (miles de pesos)")
g2_2_m <- g2_2_m + ylab("Densidad detallada")
g2_2_m <- g2_2_m + theme_pubr()
print(g2_2_m)

# Otra muy buena opci�n es usar un diagrama de densidad.
g2_3_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "#6D9EC1", fill = "#BFD5E3",
                    title = "Ingreso en la RM",
                    subtitle = "Muestra",
                    xlab = "Ingreso total (miles de pesos)", ylab = "Densidad")

g2_3_m <- ggpar(g2_3_m, xlim = c(0, 5050))
print(g2_3_m)

# Aunque tambi�n podemos ocultar los detalles con un gr�fico de cajas, al cual
# agregaremos un punto con la media. Necesitamos crear (y ocultar) un �nico
# grupo.
muestra[["grupo"]] <- 1

g2_4_m <- ggboxplot(muestra, x = "grupo", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3", title = "Ingreso en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)")

g2_4_m <- g2_4_m + scale_y_continuous(limits = c(0, 5050))
g2_4_m <- ggpar(g2_4_m, ylab = FALSE, orientation = "horizontal")
g2_4_m <- g2_4_m + rremove("y.text")
g2_4_m <- g2_4_m + rremove("y.ticks")
print(g2_4_m)

# O podemos dar m�s detalle mostrando los puntos que generan la caja.
g2_5_m <- ggboxplot(muestra, x = "grupo", y = "ytot", add = "jitter",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3", title = "Ingreso en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)")

g2_5_m <- g2_5_m + scale_y_continuous(limits = c(0, 5050))
g2_5_m <- ggpar(g2_5_m, ylab = FALSE, orientation = "horizontal")
g2_5_m <- g2_5_m + rremove("y.text")
g2_5_m <- g2_5_m + rremove("y.ticks")
print(g2_5_m)

# Los gr�ficos nos muestran que el ingreso presenta una distribuci�n asim�trica
# con valores at�picos muy altos (outliers). Esto hace que ledidas como la
# media se distorcionen, por lo que es mejor usar otras medidas de tendencia
# central m�s robustas, tales como la mediana, el rango intercuartil o la
# media truncada (en ingl�s, trimmed mean).

cat("\nMedidas de tendencia central para la muestra\n")
cat("Media:", mean(muestra[["ytot"]]), "\n")
cat("Mediana:", median(muestra[["ytot"]]), "\n")
cat("Rango intercuartil (IQR):", IQR(muestra[["ytot"]]), "\n")
cat("Media truncada (60% de los datos):", mean(muestra[["ytot"]], trim = 0.2))

# Podemos ver que la media muestral del ingreso es $589.357,4, aunque el 50% de
# las observaciones se encuentra por debajo de $ 371.666,5. Esto nos muestra que
# La mayor�a de la poblaci�n tiene ingresos muy bajos y que los valores at�picos
# ejercen una enorme influencia en la media.

# Ahora veamos qu� pasa con la poblaci�n.
# Construir gr�fico de densidad.
g2_3_p <- ggdensity(datos, x = "ytot", add = "mean", rug = TRUE,
                    color = "#6D9EC1", fill = "#BFD5E3",
                    title = "Ingreso en la RM",
                    subtitle = "Poblaci�n",
                    xlab = "Ingreso total (miles de pesos)", ylab = "Densidad")

g2_3_p <- ggpar(g2_3_p, xlim = c(0, 5050))
print(g2_3_p)

# Crear figura con los gr�ficos de la muestra y de la poblaci�n.
g2 <- ggarrange(g2_3_m, g2_3_p, ncol = 2, nrow = 1)
print(g2)

cat("\n\nMedidas de tendencia central para la poblaci�n\n")
cat("Media:", mean(datos[["ytot"]]), "\n")
cat("Mediana:", median(datos[["ytot"]]), "\n")
cat("Rango intercuartil (IQR):", IQR(datos[["ytot"]]), "\n")
cat("Media truncada (60% de los datos):", mean(datos[["ytot"]], trim = 0.2))
cat("\n\n")
cat("=========================================================================")
cat("\n")

# Las medidas robustas son m�s parecidas entre s�, para la muestra y la
# poblaci�n que la media.

# Podemos ver que la media poblacional del ingreso es $614.722,4 (�m�s alta que
# la de la muestra!), aunque el 50% de las observaciones se encuentra por debajo
# de $ 350.000 (�m�s bajo que la muestra!). Esto nos evidencia que la mayor�a de
# la poblaci�n tiene ingresos muy bajos (a�n m�s bajos de lo que anticip�bamos a
# partir de la muestra) y que hay unos pocos privilegiados cuyos ingresos son
# desmedidamente altos.



################################################################################
# AN�LISIS DE UNA VARIABLE NUM�RICA EN GRUPOS DISTINTOS
# �Tienen hombres y mujeres ingresos similares?
################################################################################

cat("\n�Tienen hombres y mujeres ingresos similares?\n")

# En este caso podemos usar los mismos gr�ficos que en el ejemplo anterior, pero
# mostrando cada grupo por separado a fin de poder comparar la "masa" o la "forma"
# de los datos. Para ello, usamos la variable categ�rica (o factor, en jerga
# estad�stica) para asignar los colores.

# Una vez m�s, comencemos con la muestra.
# Construyamos primero un gr�fico de densidad.
g3_1_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "sexo", fill = "sexo",
                    title = "Ingreso en la RM para la muestra",
                    subtitle = "(distribuciones separadas por sexo)",
                    xlab = "Ingreso total (miles de pesos)",
                    ylab = "Frecuencia")

g3_1_m <- ggpar(g3_1_m, xlim = c(0, 5050))
print(g3_1_m)

# Tambi�n podemos hacer un gr�fico de cajas.
g3_2_m <- ggboxplot(muestra, x = "sexo", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "sexo", title = "Ingreso en la RM para la muestra",
                    subtitle = "(cajas y medias por sexo)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Sexo")

g3_2_m <- g3_2_m + scale_y_continuous(limits = c(0, 5050))
g3_2_m <- ggpar(g3_2_m, orientation = "horizontal")
print(g3_2_m)

# Los gr�ficos sugieren que las mujeres tienen ingresos algo m�s bajos que los
# hombres.

# Nos sirven en este caso los mismos estad�sticos que en la pregunta anterior,
# pero ahora para cada grupo por separado. La asimert�a en la distribuci�n de
# los datos (gr�fico de densidad), que tambi�n podemos ver como la presencia de
# valores at�picos (el gr�fico de cajas), sugiere que usemos medidas de
# tendencia central m�s robustas que la media.
cat("\nMedidas de tendencia central de la muestra para hombres y mujeres\n")
cat("Media mujeres:", mean(muestra[muestra[["sexo"]] == "Mujer", "ytot"]), "\n")

cat("Media hombres:", mean(muestra[muestra[["sexo"]] == "Hombre", "ytot"]),
    "\n")

cat("Mediana mujeres:", median(muestra[muestra[["sexo"]] == "Mujer", "ytot"]),
    "\n")

cat("Mediana hombres:", median(muestra[muestra[["sexo"]] == "Hombre", "ytot"]),
    "\n")

cat("IQR mujeres:", IQR(muestra[muestra[["sexo"]] == "Mujer", "ytot"]), "\n")
cat("IQR hombres:", IQR(muestra[muestra[["sexo"]] == "Hombre", "ytot"]), "\n")

# Al comparar el ingreso medio de hombres y mujeres, vemos que los primeros
# son bastante m�s altos, diferencia que se acrecenta si miramos las medianas.

# Veamos ahora qu� ocurre con la poblaci�n.
# Comencemos por el gr�fico de cajas.
g3_2_p <- ggboxplot(datos, x = "sexo", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "sexo",
                    title = "Ingreso en la RM para la poblaci�n",
                    subtitle = "(cajas y medias por sexo)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Sexo")

g3_2_p <- g3_2_p + scale_y_continuous(limits = c(0, 5050))
g3_2_p <- ggpar(g3_2_p, orientation = "horizontal")
print(g3_2_p)

# Obtenemos ahora las medidas de tendencia central.
cat("\nMedidas de tendencia central de la poblaci�n para hombres y mujeres\n")
cat("Media mujeres:", mean(datos[datos[["sexo"]] == "Mujer", "ytot"]), "\n")
cat("Media hombres:", mean(datos[datos[["sexo"]] == "Hombre", "ytot"]), "\n")
cat("Mediana mujeres:", median(datos[datos[["sexo"]] == "Mujer", "ytot"]), "\n")

cat("Mediana hombres:", median(datos[datos[["sexo"]] == "Hombre", "ytot"]),
    "\n")

cat("IQR mujeres:", IQR(datos[datos[["sexo"]] == "Mujer", "ytot"]), "\n")
cat("IQR hombres:", IQR(datos[datos[["sexo"]] == "Hombre", "ytot"]), "\n")

# Generamos la figura con ambos gr�ficos.
g3 <- ggarrange(g3_2_m, g3_2_p, ncol = 2, nrow = 1)
print(g3)
cat("\n")
cat("=========================================================================")
cat("\n")

# Tristemente, una vez m�s vemos que, en la poblaci�n, la diferencia entre los
# ingresos de hombres y mujeres es a�n m�s acentuada que lo que vimos en la
# muestra.



################################################################################
# AN�LISIS DE UNA VARIABLE NUM�RICA EN GRUPOS DISTINTOS
# �Tienen las personas que viven en �reas rurales ingresos similares a quienes
# viven en �reas urbanas?
################################################################################

cat("\n�Tienen las personas que viven en �reas rurales ingresos similares a ")
cat("quienes viven en �reas urbanas?\n")

# El an�lisis es el mismo que en la pregunta anterior.

# Una vez m�s, comencemos con la muestra.
# Construyamos primero un gr�fico de densidad.
g4_1_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "zona", fill = "zona",
                    title = "Ingreso en la RM (muestra)",
                    subtitle = "(distribuciones separadas por zona)",
                    xlab = "Ingreso total (miles de pesos)",
                    ylab = "Frecuencia")

g4_1_m <- ggpar(g4_1_m, xlim = c(0, 5050))
print(g4_1_m)

# Veamos ahora el gr�fico de cajas.
g4_2_m <- ggboxplot(muestra, x = "zona", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "zona", title = "Ingreso en la RM (muestra)",
                    subtitle = "(cajas y medias por zona)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Zona")

g4_2_m <- g4_2_m + scale_y_continuous(limits = c(0, 5050))
g4_2_m <- ggpar(g4_2_m, orientation = "horizontal")
print(g4_2_m)

# Obtenemos ahora las medidas de tendencia central.
cat("\nMedidas de tendencia central de la muestra, zonas urbana y rural\n")
cat("Media urbana:", mean(muestra[muestra[["zona"]] == "Urbano", "ytot"]), "\n")
cat("Media rural:", mean(muestra[muestra[["zona"]] == "Rural", "ytot"]), "\n")

cat("Mediana urbana:", median(muestra[muestra[["zona"]] == "Urbano", "ytot"]),
    "\n")

cat("Mediana rural:", median(muestra[muestra[["zona"]] == "Rural", "ytot"]),
    "\n")

cat("IQR urbana:", IQR(muestra[muestra[["zona"]] == "Urbano", "ytot"]), "\n")
cat("IQR rural:", IQR(muestra[muestra[["zona"]] == "Rural", "ytot"]), "\n")

# Resulta interesante ver que, si bien la media es m�s alta para zonas urbanas,
# la mediana es m�s elevada en las zonas rurales. Pero �qu� tan confiables
# ser�n estos resultados?

# Agreguemos m�s detalles al gr�fico de cajas.
g4_3_m <- ggboxplot(muestra, x = "zona", y = "ytot", add = "jitter",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "zona", title = "Ingreso en la RM (muestra)",
                    subtitle = "(cajas y puntos por zona)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Zona")

g4_3_m <- g4_3_m + scale_y_continuous(limits = c(0, 5050))
g4_3_m <- ggpar(g4_3_m, orientation = "horizontal")
print(g4_3_m)

# Nuestra muestra tiene muy pocos casos correspondientes a �reas rurales, por lo
# que no podemos asegurar que el comportamiento que vemos en los gr�ficos sea
# representativo de la poblaci�n.

# Aunque rara vez tendremos tanta suerte, �esta vez conocemos la poblaci�n!
# Creamos el gr�fico de cajas.
g4_2_p <- ggboxplot(datos, x = "zona", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "zona", title = "Ingreso en la RM (poblaci�n)",
                    subtitle = "(cajas y medias por zona)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Zona")

g4_2_p <- g4_2_p + scale_y_continuous(limits = c(0, 5050))
g4_2_p <- ggpar(g4_2_p, orientation = "horizontal")
print(g4_2_p)

# Y ahora calculamoslas medidas de tendencia central.
cat("\nMedidas de tendencia central de la poblaci�n, zonas urbana y rural\n")
cat("Media urbana:", mean(datos[datos[["zona"]] == "Urbano", "ytot"]), "\n")
cat("Media rural:", mean(datos[datos[["zona"]] == "Rural", "ytot"]), "\n")
cat("Mediana urbana:", median(datos[datos[["zona"]] == "Urbano", "ytot"]), "\n")
cat("Mediana rural:", median(datos[datos[["zona"]] == "Rural", "ytot"]), "\n")
cat("IQR urbana:", IQR(datos[datos[["zona"]] == "Urbano", "ytot"]), "\n")
cat("IQR rural:", IQR(datos[datos[["zona"]] == "Rural", "ytot"]), "\n")

# Podemos observar una enorme diferencia etre los ingresos de zonas rurales y
# urbanas, y que el valor central para las zonas rurales no alcanza siquiera los
# $300.000.

# Generamos la figura con ambos gr�ficos.
g4 <- ggarrange(g4_2_m, g4_2_p, ncol = 2, nrow = 1)
print(g4)
cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# AN�LISIS DE UNA VARIABLE NUM�RICA EN GRUPOS DISTINTOS
# �Son similares los ingresos registrados en las diferentes provincias de la RM?
################################################################################

cat("\n�Son similares los ingresos registrados en las diferentes provincias ")
cat("de la RM?\n")

# El an�lisis para responder esta pregunta es similar al de las preguntas
# precedentes, aunque ahora tenemos m�s de dos grupos. En consecuencia, nos
# centraremos �nicamente en aquellos aspectos que aporten algo nuevo.

# Construyamos primero un gr�fico de densidad para la muestra.
g5_1_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "provincia", fill = "provincia",
                    title = "Ingreso en la RM (muestra)",
                    subtitle = "(distribuciones separadas por provincia)",
                    xlab = "Ingreso total (miles de pesos)",
                    ylab = "Frecuencia")

g5_1_m <- ggpar(g5_1_m, xlim = c(0, 5050))
print(g5_1_m)

# Veamos ahora el gr�fico de cajas.
g5_2_m <- ggboxplot(muestra, x = "provincia", y = "ytot", color = "provincia",
                    title = "Muestra del ingreso en la Regi�n Metropolitana",
                    subtitle = "(cajas y puntos por provincia)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Provincia")

g5_2_m <- g5_2_m + scale_y_continuous(limits = c(0, 5050))
g5_2_m <- ggpar(g5_2_m, legend = "right", legend.title = "Provincias de\nla RM")
print(g5_2_m)

# Pero una vez m�s debemos verificar la validez de la muestra.
g5_3_m <- ggboxplot(muestra, x = "provincia", y = "ytot", color = "provincia",
                    add = "jitter",
                    title = "Muestra del ingreso en la Regi�n Metropolitana",
                    subtitle = "(cajas y puntos por provincia)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Provincia")

g5_3_m <- g5_3_m + scale_y_continuous(limits = c(0, 5050))
g5_3_m <- ggpar(g5_3_m, legend = "right", legend.title = "Provincias de\nla RM")
print(g5_3_m)

# En este caso puede ser arriesgado inferir con la muestra obtenida, pues
# hay grupos con muy pocas observaciones.

cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# AN�LISIS DE DOS VARIABLES NUM�RICAS
# �Tiene relaci�n el ingreso con la riqueza del municipio donde se habita?
################################################################################

cat("\n�Tiene relaci�n el ingreso con la riqueza del municipio donde se ")
cat("habita?\n")

# En este caso, queremos ver si una variable num�rica influye sobre otra
# variable (tambi�n num�rica):
# - Si una variable crece, �la otra tambi�n?
# - O bien, si una bariable crece, �disminuye la otra?
# - O tal vez no hay relaci�n, es decir, las variables son independientes.

# Una herramienta adecuada para abordar este tipo de preguntas es el gr�fico de
# dispersi�n.

# Como siempre, comencemos por estudiar la muestra.
# Crear gr�fico de dispersi�n.
g6_1_m <- ggscatter(muestra, x = "ing.comuna", y = "ytot", color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relaci�n ingreso - riqueza de la comuna en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_1_m <- g6_1_m + scale_y_continuous(limits = c(0, 5050))
print(g6_1_m)

# Parecer�a que hay una relaci�n positiva, aunque leve.

# Agregamos una l�nea de tendencia para ver la relaci�n de forma m�s clara.
g6_2_m <- ggscatter(muestra, x = "ing.comuna", y = "ytot", add = "reg.line",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relaci�n ingreso - riqueza de la comuna en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_2_m <- g6_2_m + scale_y_continuous(limits = c(0, 5050))
print(g6_2_m)

# Ahora s� es claro que si una variable crece, la otra tambi�n lo hace. Aunque
# la relaci�n no parece muy fuerte.

# Veamos ahora qu� pasa con la poblaci�n.
g6_2_p <- ggscatter(datos, x = "ing.comuna", y = "ytot", add = "reg.line",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relaci�n ingreso - riqueza de la comuna en la RM",
                    subtitle = "Poblaci�n",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_2_p <- g6_2_p + scale_y_continuous(limits = c(0, 5050))
print(g6_2_p)

# En efecto, hay una relaci�n leve.

cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# AN�LISIS DE DOS VARIABLES NUM�RICAS EN GRUPOS DISTINTOS
# �Van los ingresos de los chilenos increment�ndose con la experiencia y de
# forma similar entre hombres y mujeres?
################################################################################

cat("\n�Van los ingresos de los chilenos increment�ndose con la experiencia y ")
cat("de forma similar entre hombres y mujeres?\n")

# Procedemos en forma similar a la pregunta anterior, pero ahora separamos los
# diferentes grupos por colores.

# Construimos primero el gr�fico de dispersi�n para la muestra.
g7_1_m <- ggscatter(muestra, x = "edad", y = "ytot", add = "reg.line",
                    add.params = list(color = "sexo"), color = "sexo",
                    fill = "sexo", title = "Relaci�n ingreso - edad en la RM",
                    subtitle = "(Muestra, separada por sexo)",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Edad (a�os)")

g7_1_m <- g7_1_m + scale_y_continuous(limits = c(0, 5050))
print(g7_1_m)

# Parecer�a que, en el caso de las mujeres, no hay relaci�n entre ambas
# variables. En el caso de los hombres, en cambio, el ingreso parece aumentar
# levemente con la edad.

# Veamos ahora qu� pasa con la poblaci�n.
g7_1_p <- ggscatter(datos, x = "edad", y = "ytot", add = "reg.line",
                    add.params = list(color = "sexo"), color = "sexo",
                    fill = "sexo", title = "Relaci�n ingreso - edad en la RM",
                    subtitle = "(Poblaci�n, separada por sexo)",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Edad (a�os)")

g7_1_p <- g7_1_p + scale_y_continuous(limits = c(0, 5050))
print(g7_1_p)

# Podemos ver que, a medida que aumentan los a�os de experiencia, los hombres
# ven sus ingresos incrementados, mientras que las mujeres no.