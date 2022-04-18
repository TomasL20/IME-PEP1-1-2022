library(dplyr)

#cargar conjunto de datos
datos <- mtcars

#renombrar columnas
datos <- datos %>% rename(Rendimiento = mpg, Cilindrada = cyl,
                          Desplazamiento = disp, Potencia = hp,
                          Eje = drat, Peso = wt, Cuarto_milla = qsec,
                          Motor = vs, Transmision = am, Cambios = gear,
                          Carburadores = carb)
#dar formato categorico a las variables motor y transmision, renombrando
#sus variables
datos[["Motor"]] <- factor(dator[["Motor"]], levels = c(0,1),
                           labels = c("V", "Recto"))
datos [[" Transmision "]] <- factor ( datos [[" Transmision "]] , levels = c(0 , 1) ,
                                       labels = c(" Autom á tico ", " Manual ") )

# Dar formato ordinal a las variables Cilindrada y Cambios , renombrando
# sus niveles .
datos [[" Cilindrada "]] <- factor ( datos [[" Cilindrada "]] , levels = c(4 , 6 , 8) ,
                                    labels = c("4 cilindros ", "6 cilindros ",
                                      "8 cilindros ") ,
                                       ordered = TRUE )

datos [[" Cambios "]] <- factor ( datos [[" Cambios "]] , levels = c(3 , 4 , 5) ,
                                  labels = c("3 cambios ", "4 cambios ", "5 cambios ") ,
                                     ordered = TRUE )

#calcular la media para la variable Rendimiento
media <- mean (datos[["Rendimiento"]])
cat("Rendimiento medio:", media, "\n\n")

#calcular la media para la tercera y quinta columnas
# (variables Desplazamiento y Eje)
cat("medias \n")
print(sapply(datos[c(3,5)], mean))
cat("\n")

#calcular la media para las columnas 3 a 6
#variables (desplazamiento, potencia, eje y peso)
cat("medias\n")
print(sapply(datos[3:6], mean))
cat("\n")

#calcular la media para la variable Rendimiento omitiendo valores faltantes
print(mean(datos[["Rendimiento"]], na.rm = TRUE))


#calculo de percentiles para la variable rendimiento
cat("Cuartiles: \n")
print(quantile(datos[["Rendimiento"]]))
cat("\n")

#calculo de varias medidad para la variable potencia
medidas_potencia <- datos %>% summarise(Media = mean(Potencia),
                                        Mediana = median(Potencia),
                                        Varianza = var(Potencia),
                                        IQR = IQR(Potencia))
print(medidas_potencia)
cat("\n")


#crear la tabla de contingencia para la variable gear
contingencia <- table(datos[["Cambios"]])
cat("tabla de contingencia generada con table() \n")
print(contingencia)
cat("\n")

#otra forma de crear la misma tabla
contingencia_2 <- xtabs(~Cambios, data = datos)
cat("tabla de contingencia creada con xtabs() \n")
print(contingencia_2)

#calcular totales por fila y mostrarlos por separado
totales <- marginSums(contingencia)
cat("totales por fila: \n")
print(totales)

#calcular totales por fila y agregarlos a la tabla
con_totales <- addmargins(contingencia,1)
cat("tabla de contingencia con totales por fila \n")
print(con_totales)
cat("\n")

# Convertir a tabla de proporciones
proporciones <- prop.table( contingencia )
proporciones <- addmargins ( proporciones , 1)
cat (" Tabla de contingencia con proporciones :\n")
print ( proporciones )
cat ("\n")


 # Convertir a tabla de porcentajes con 2 decimales .
 porcentajes <- round ( prop.table ( contingencia ) , 4) * 100
 porcentajes <- addmargins ( porcentajes )
 cat (" Tabla de contingencia con porcentajes :\n")
 print ( porcentajes )
 cat ("\n")

 # Convertir a tabla de porcentajes con 2 decimales .
porcentajes <- round ( prop.table ( contingencia ) , 4) * 100
porcentajes <- addmargins ( porcentajes )
cat (" Tabla de contingencia con porcentajes :\n")
print ( porcentajes )
cat ("\n")
 
# Crear tabla de contingencia para las variables Transmision y gear .
contingencia <- table ( datos [[" Transmision "]] , datos [[" Cambios "]])
cat (" Tabla de contingencia generada con table () :\n")
print ( contingencia )
cat ("\n")

# Otra forma de crear la misma tabla .
contingencia <- xtabs (~ Transmision + Cambios , data = datos )
cat (" Tabla de contingencia generada con xtabs () :\n")
print ( contingencia )
cat ("\n")

# Convertir la variable Cambios en categ ó rica .
datos [[" Cambios "]] <- factor ( datos [[" Cambios "]])

# Crear tabla de contingencia para las variables Transmision ,
# Cambios y Motor .
contingencia <- ftable ( datos [[" Transmision "]] , datos [[" Cambios "]] ,
                             datos [[" Motor " ]])

cat (" Tabla de contingencia generada con ftable () :\n")
print ( contingencia )
cat ("\n")

# Otra forma de crear la misma tabla .
xtabs (~ Cambios + Transmision + Motor , data = datos )
cat (" Tabla de contingencia generada con xtabs () :\n")
print ( contingencia )
cat ("\n")

resumen <- group_by( datos , Cambios ) %>%
summarise ( count = n () , mean ( Rendimiento ) , median ( Rendimiento ) ,
sd( Rendimiento ) , IQR ( Rendimiento ) , mean ( Potencia ) )
print ( resumen )

#graficos
library(ggpubr)
#histograma para la variable rendimiento
g_1 <- gghistogram (datos, 
                   x = "Rendimiento", 
                   bins = 10,
                   add = "mean",
                   xlab = "rendimiento [millas/galon]",
                   ylab = "frecuencia",
                   color = "blue",
                   fill = "blue")
print(g_1)

# Histograma para la variable Potencia .
 g_2 <- gghistogram (datos,
                      x = "Potencia",
                      bins = 10 ,
                      add = "mean",
                      xlab = "Potencia [hp]",
                      ylab = "Frecuencia ",
                      color = "red",
                      fill = " yellow")

 print ( g_2 )
 
#grafico de caja y bigotes 
 g_3 <- ggboxplot(datos[[" Potencia "]],
                  color = "red",
                  fill = "pink",
                  ylab = "Potencia [hp]")
 
g_3 <- g_3 + rremove ("x.ticks ")
g_3 <- g_3 + rremove ("x.text ")
g_3 <- g_3 + rremove ("x.title ")
 
print (g_3 )
library(ggpubr)
#grafico de barras
# Crear la tabla de frecuencias para la variable Cambios y convertirla a
# data frame .
contingencia_4 <- as.data.frame( xtabs (~ Cambios , data = datos ) )

# Crear el gráico de barras .
g_4 <- ggbarplot ( contingencia_4 ,
                 x = "Cambios",
                 y = "Freq",
                 fill = c("brown", "purple", "orange") ,
                title = "Cantidad de cambios de los autom ó viles ",
                xlab = "Cantidad de cambios ",
                ylab = "Frecuencia ")

 print(g_4)
 
 #grafico de torta
 # Crear la tabla de frecuencias y convertirla a data frame .
 contingencia_5 <- as.data.frame ( xtabs (~ Cambios , data = datos ) )
 
 # Crear grá fico de torta .
 g_5 <- ggpie (contingencia_5,
                  x = "Freq",
                 label = "Cambios",
                 fill = c("red", "yellow ", "green") ,
                 title = " Cantidad de cambios de los autom ó viles ",
                 lab.pos = "in")
 
 
print (g_5 )

#grafico de dispersion
# Crear grá fico de dispersi ón.
g_6 <- ggscatter (datos ,
                   x = "Rendimiento",
                   y = "Peso",
                   color = "red",
                   title = "Rendimiento v/s peso ",
                   xlab = "Rendimiento [ millas /galón]",
                   ylab = "Peso [1000 lb]")
print (g_6)
