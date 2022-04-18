#metodo de wald para una proporcion
n<- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7

#construccion del intervalo de confianza
error_est <- sqrt((p_exito*(1-p_exito))/n)
Z_critico <- qnorm(alfa/2, lower.tail = FALSE)
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico *error_est
cat("intervalo de confianza = [", inferior,",",superior,"]'n", sep = " ")

#prueba de hipotesis
error_est_hip <- sqrt((valor_nulo * (1-valor_nulo)) / n)
Z <- (p_exito-valor_nulo)/error_est_hip
p <- pnorm(Z, lower.tail=FALSE)
cat("hipotesis alternativa unilateral \n")
cat("Z= ", Z, "\n")
cat("p =", p)
#------------------------------------------------------------------
#metodo de wald para la diferencia entre dos proporciones
n_hombres <- 48
n_mujeres <- 42
exitos_hombres <-26
exitos_mujeres <- 20
alfa <- 0.05
valor_nulo <- 0
#calcular probabilidades de exito
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres

#estimar la diferencia 
diferencia <- p_hombres - p_mujeres
#construccion del intervalo de confianza
error_hombres <- (p_hombres * (1-p_hombres))/n_hombres
error_mujeres <- (p_mujeres * (1-p_mujeres))/n_mujeres
error_est <- sqrt(error_hombres+error_mujeres)
Z_critico <- qnorm(alfa/2, lower.tail = FALSE)
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat("intervalo de confianza= [", inferior," , ", superior, "]/n")

#prueba de hipotesis

p_agrupada <- (exitos_hombres + exitos_mujeres)/ (n_hombres + n_mujeres)
error_hombres <- (p_agrupada *(1-p_agrupada))/n_hombres
error_mujeres <- (p_agrupada * (1-p_agrupada)) / n_mujeres
Z<- (diferencia -valor_nulo) / error_est_hip
p <- 2 * pnorm(Z,lower.tail = FALSE) #area bajo la curva
cat("hipotesis alternativa bilateral")
cat("Z=", Z, "\n")
cat("p = ", p,"\n")

#-----------------------------------------------------------------
#metodo de wald para la diferencia entre dos proporciones
#fijar valores conocidos
n_hombres <- 89
n_mujeres <- 61
exito_h <- 45
exito_m <- 21
alfa <- 0.05
valor_nulo <- 0.1

#calcular probabilidades de exito
p_h<- exito_h / n_hombres
p_m <- exito_m / n_mujeres

#estimar la diferencia
diferencia <- p_h - p_m

#prueba de hipotesis
p_agrupada <- (exito_m + exito_m)/(n_hombres + n_mujeres)
error_h <- (p_h *(1-p_h))/n_hombres
error_m <- (p_m * (1-p_m)) /n_mujeres
error_est <- sqrt(error_h + error_m)
Z<- (diferencia-valor_nulo) / error_est
p <- pnorm(Z, lower.tail=FALSE)
cat("hipotesis alternativa bilateral\n")
cat("Z= ",Z,"\n")
cat("p= ", p,"\n")
#----------------------------------------------------------------
# metodo de wilson para una proporcion
#fijar valores conocidos
n <- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7

#calculoar cantidad de exitos
exitos <- p_exito * n

#prueba de Wilson en R
prueba_W <- prop.test(exitos, n=n, p=valor_nulo,
                    alternative = "greater", conf.level = 1-alfa)
print(prueba_W)

