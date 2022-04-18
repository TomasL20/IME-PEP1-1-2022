#Equipo 7

#Integrantes:
#Tomás López
#Adolfo Navarrete
#Fabián Sepúlveda

#Preguntas
#1 se han cargado las variables de los nombres de las regiones y el número de casos con sintomas en cada día 
#2 los nombres de las regiones son de tipo catégorica y el número de casos por día es de tipo númerica y en este caso es discreta
#3 los nombres de las regiones son de escala nominal y el número de casos por día es de escala de razón puesto que existe un 0 verdadero

#Leemos los datos (Ajustar la direccion del archivo .csv segun donde este ubicado)
datos <- read.csv2("C:\\IME\\EJ1\\EP01 Datos Covid.csv")

#Importamos el paquete dplyr
library(dplyr)
library(tidyr)
#Leemos los datos de los rios
datosLosRios <- datos %>% filter(Region == "Los Ríos")

#Leemos los datos segun las fechas indicadas
datosFecha <- datosLosRios[c(92:305)]

#obtenemos el máximo
max <- max(datosFecha,  na.rm = FALSE)
