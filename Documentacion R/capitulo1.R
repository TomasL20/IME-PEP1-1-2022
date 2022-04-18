#crear un vector de string y guardarlo en la variable nombre
nombre <- c("Alan brito delgado",
            "Zacarias labarca del rio",
            "Elsa payo maduro")

#crear un vector de fechas y guardarlo en la variable
#fecha_nacimiento
fecha_nacimiento <- as.Date(c("2008-1-25","2006-10-4","2008-3-27"))

#crear tres vectores de reales entre 1.0 y 7.0 y guardalos
#en prueba_i , respectivamente
prueba_1 <- c(5.5, 3.4, 4.5)
prueba_2 <- c(3.2, 4.7, 4.1)
prueba_3 <- c(4.8, 4.3, 5.1)

#construir un data frame a partir de los vectores anteriores y 
#guardarlo en la variable dataframe
dataframe <- data.frame(nombre,
                        fecha_nacimiento,
                        prueba_1,
                        prueba_2,
                        prueba_3,
                        stringsAsFactors = FALSE)




library(dplyr)
#cargar dataframe iris incluido en R
datos <- iris

#seleccionar observaciones correspondientes a la especie versicolor
versicolor <- datos %>%filter(Species == "versicolor")

print(versicolor)
#seleccionar observaciones de la especie versicolor cuyos sepalos tengan
#una longifut igual o superior a 6 cm

largas <- datos %>%filter(Species == "versicolor" & Sepal.Length >=6)

#seleccionar la especie y variables relativas a los petalos
petalos <- datos %>% select(Species, starts_with("Petal"))

#seleccionar variables de ancho y la especie
anchos <- datos %>% select(ends_with("With"), Species)

#agregar al conjunto de datos de los petalos una nueva variable con la razon
#entre el largo y el ancho de estos
petalos <- petalos %>% mutate (Species, Petal.Width,
                               Petal.Ratio = Petal.Length / Petal.Width)
#ordenar el conjunto de datos de petalos en forma descendete segun la razon 
#de los petalos
petalos <- petalos %>% arrange(desc(Petal.Ratio))

#Ordenar el conjunto de datoa de petalos en forma ascendete segun el largo de
#los petalos
petalos <- petalos %>% arrange(Petal.Length)






