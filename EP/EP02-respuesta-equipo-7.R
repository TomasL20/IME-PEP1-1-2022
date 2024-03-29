#Equipo 7

#Integrantes:
#Tom�s L�pez
#Adolfo Navarrete
#Fabi�n Sep�lveda

#�Tiene relaci�n el ingreso de las mujeres de la RM con el riqueza del municipio donde habita?

#PROCEDIMIENTO
#En primera instancia nos interesa aislar la poblaci�n requerida, en esta ocasi�n tenemos
# a las mujeres que viven dentro de la regi�n metropolitana
#En segundo debemos obtener un promedio de ganancias dentro de la comuna, por ello
# se utiliz� la funci�n mean, de esta forma obtenemos los ingresos promedios por mujeres en la
# comuna
#Finalmente en base a lo indicado en los datos entregados tenemos la columna "ing.comuna", que
# consiste en un ranking de ingresos, por ello se realiza una comparaci�n, en base
# al promedio de ganancia y la posici�n que se indica en este ranking

#De esta forma podemos realizar la comparativa a la situaci�n requerida para responder la pregunta
# planteada, haciendo un contraste entre si la posici�n en este ranking corresponde al promedio de 
# ganancias de las mujeres en la comuna

#GRAFICA
# Para la gr�fica se opt� por una gr�fico de dispersi�n, esto debido a que se trabaja
# con dos variables num�ricas


#Importamos las librerias
library(dplyr)
library(ggpubr)

#Leemos los datos (Cambiar seg�n la ubicaci�n del archivo .csv)
datos <- read.csv2("C:\\Users\\fabia\\Desktop\\R\\EP02\\EP02 Datos Casen 2017.csv", encoding = "UTF-8")

#Filtramos por la region y el sexo requerido
datosFilt <- datos %>% filter(region == "Regi�n Metropolitana de Santiago" & sexo == "Mujer")

#Agrupamos por los ingresos de la comuna y la comuna
#para luego obtener un promedio de cada comuna en base a los ingresos
datosComunas <- datosFilt %>% group_by(ing.comuna, comuna) %>% summarise(prom = mean(ytot)) 

#Finalmente gr�ficamos
g <- ggscatter(datosComunas,
               x = "ing.comuna",
               y = "prom",
               color = "blue",
               title = "Ranking vs Ingresos promedio",
               xlab = "Ranking de la comuna",
               ylab = "Ingreso Promedio mujer por Comuna")
#mostramos la gr�fica en la pantalla
print(g)


