#prueba chi-cuadrado de homogeneidad
#crear tabla de contingencia
programadores <- c(42,56,51,27,25)
programadoras <- c(25,24,27,15,9)
tabla <- as.table(rbind(programadores, programadoras))
dimnames(tabla) <- list (sexo = c("programadores", "programadoras"),
                                  lenguajes = c("C", "Java", "Python", "Ruby", "Otro"))
print(tabla)
#hacer prueba chi-cuadrado de homogeneidad
prueba <- chisq.test(tabla)
print(prueba)
#--------------------------------------------------------------------------
#prueba chi-cuadrado de bondad de ajuste
nomina <- c(236,78,204,76,66)
muestra <- c(17,9,14,10,5)
tabla <- as.table(rbind(nomina, muestra))
dimnames(tabla) <- list ( grupo = c("Nó mina ", " Muestra "),
                          lenguajes = c("C", " Java ", " Python ", " Ruby ", " Otro "))
print(tabla)
#verificar si se esperan mas de 5 observaciones por cada grupo
n_nomina <- sum(nomina)
n_muestra <- 55
proporciones <- round(nomina/n_nomina, 3)
esperados <- round(proporciones*n_muestra, 3)
print(esperados)

#hacer la prueba chi-cuadrado de homogeneidad
prueba <- chisq.test(tabla, correct=FALSE)
print(prueba)
#----------------------------------------------------------------
#prueba chi-cuadrado de independencia 
#crear tabla de contingencia
comestible <- c(404,1948, 32, 228, 1596)
venenoso <- c(48, 1708,0,600,1556)

tabla <- as.table(rbind(comestible,venenoso))
dimnames ( tabla ) <- list ( tipo = c(" comestible ", " venenoso "),
                             sombrero = c(" campana ", " convexo ", " hundido ",
                                           " nudoso ", " plano "))

print ( tabla )

#hacer prueba chi-cuadrado de independencia 
prueba <- chisq.test(tabla)
cat("la prueba internamente calcula los valores esperados: \n")
esperados <- round(prueba[["expected"]],3)
print(esperados)

cat("resultado de la prueba")
print(prueba)
#----------------------------------------------------------------
#prueba exacta de fisher
#construir la tabla de contingencia
vacuna <- c(rep (" Argh ", 6) , rep (" Grrr ", 11))
resultado <- c(rep(" Humano ", 12) , rep(" Vampiro ", 5))
datos <- data.frame ( resultado , vacuna )
tabla <- xtabs (~. , datos )
print ( tabla )
#aplicar prueba exacta de Fisher
alfa <- 0.05
prueba <- fisher.test(tabla, 1-alfa)
print(prueba)

#------------------------------------------------------------------
#prueba McNemar
alumno <- seq(1:25)
modelo_1 <- c(rep(" Correcto ", 16) , rep(" Incorrecto ", 9))
modelo_2 <- c(rep(" Correcto ", 9) , rep(" Incorrecto ", 11) , rep(" Correcto ", 5))
datos <- data.frame (alumno , modelo_2, modelo_1)
tabla <- table ( modelo_2, modelo_1)
print ( tabla )

#aplicar prueba de McNemar
prueba <- mcnemar.test(tabla)
print(prueba)

#------------------------------------------------------------------
#prueba Q de Cochran
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)
#crear matriz de datos
instancia <- 1:15
annealing <- c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
hormigas <- c(0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1)
genetico <- c(1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1)
datos <- data.frame ( instancia , annealing , hormigas , genetico )

# Llevar matriz de datos a formato largo .
datos <- datos %>% pivot_longer (c("annealing", "hormigas", "genetico"),
                                       names_to = "metaheuristica",
                                       values_to = "resultado")

datos[["instancia"]] <- factor ( datos [["instancia"]])
datos[["metaheuristica"]] <- factor ( datos [["metaheuristica"]])
# Hacer prueba Q de Cochran .
prueba <- cochran.qtest( resultado~metaheuristica|instancia ,
                          data = datos , alpha = 0.05)

print ( prueba )

# Procedimiento post -hoc con corrección de Bonferroni .

post_hoc_1 <- pairwiseMcnemar ( resultado~metaheuristica|instancia ,
                                data = datos , method = "bonferroni")

cat ("\ nProcedimiento post - hoc con correcci ón de Bonferroni \n")
print ( post_hoc_1)

# Procedimiento post -hoc con correcci ón de Holm .
post_hoc_2 <- pairwiseMcnemar ( resultado~metaheuristica|instancia ,
                                data = datos , method = "holm")

cat ("\ nProcedimiento post - hoc con correcci ón de Holm \n")
print ( post_hoc_2)
