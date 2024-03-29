#Equipo 7

#Integrantes:
#Tom�s L�pez
#Adolfo Navarrete
#Fabi�n Sep�lveda

#Preguntas
#1 se han cargado las variables de los nombres de las regiones y el n�mero de casos con sintomas en cada d�a 
#2 los nombres de las regiones son de tipo cat�gorica y el n�mero de casos por d�a es de tipo n�merica y en este caso es discreta
#3 los nombres de las regiones son de escala nominal y el n�mero de casos por d�a es de escala de raz�n puesto que existe un 0 verdadero

#Leemos los datos (Ajustar la direccion del archivo .csv segun donde este ubicado)
datos <- read.csv2("C:\\IME\\EJ1\\EP01 Datos Covid.csv")

#Importamos el paquete dplyr
library(dplyr)
library(tidyr)
#Leemos los datos de los rios
datosLosRios <- datos %>% filter(Region == "Los R�os")

#Leemos los datos segun las fechas indicadas
datosFecha <- datosLosRios[c(92:305)]

#obtenemos el m�ximo
max <- max(datosFecha,  na.rm = FALSE)
