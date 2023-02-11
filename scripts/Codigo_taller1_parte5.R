#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 06/02/2023 
#
#  Objetivo del script: Este código corresponde al punto 5
#_____________________________________________________________________________#

# - Librerias

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer)

# - Fijar el Seed 
set.seed(10101)


# - Punto 5A

# Crear una variable ID para la división de la base: train y test 

GEIH$id <- 1:nrow(GEIH)


# Dividir la base en 70% (train), 30% (test)

train <- GEIH %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(GEIH, train, by = 'id')



# - Punto 5B

# Modelo base solo con la constante 
mod1 <- lm(log_salario_m~1 , data = train)

# Modelos punto 3
mod2 <- lm(log_salario_m~edad + edad_2, data = train)

# Modelos punto 4
mod3 <- lm(log_salario_m~ mujer , data = train )

mod4 <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                 + edad_2 + informal, data = train)

# Nuevos modelos 

mod5 <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                 + edad_2 + informal + estrato + cabecera, data = train)

mod6 <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                 + edad_2 + informal + estrato + cabecera + media + superior , data = train)

mod7 <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                 + edad_2 + informal + estrato + cabecera + media + superior 
                 + exp_trab_actual  , data = train)

mod8 <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                 + edad_2 + informal + estrato + cabecera + media + superior 
                 + exp_trab_actual + oficio , data = train)


# Variables del 8 modelo 

GEIH$edad_3 <- GEIH$edad^3
GEIH$horas_trab_usual_2 <- GEIH$horas_trab_usual^2
GEIH$exp_trab_actual_2 <- GEIH$exp_trab_actual^2

#mod8_p5 <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                # + edad_2 + informal + estrato + cabecera + media + superior 
                # + exp_trab_actual + oficio + edad_3 + horas_trab_usual_2 
                 #+ exp_trab_actual_2 , data = train)

#stargazer(mod0_p5, mod1_p5 , mod2_p5 , mod3_p5 , type = "text")
#stargazer(mod4_p5, mod5_p5 , mod6_p5 , mod7_p5 , type = "text")       
          

#test$mod3 <- predict(mod3,newdata = test)
#MSE_mod3  <- with(test,mean((log_salario_m-mod3)^2))


MSE_mod <- numeric(5) 
for (i in 1:5) {
  modelo <- get(paste0("mod", i)) 
  prediction <- predict(modelo,newdata = test)
  test[[paste0("mod", i)]] <- prediction
  MSE_mod[i] <- with(test, mean((log_salario_m-prediction)^2))
}
which.min(MSE_mod)

# Dado que el mejor modelo es el 5,  guardamos la predicción y el verdadero valor 

test$mejormodelo <- predict(mod5 ,newdata = test)
test_subset <- subset(test, select = c("log_salario_m", "mejormodelo"))


par(mfrow=c(1,2))
plot(density(test_subset$log_salario_m), main="Distribución de valores verdaderos", col="red", xlab="Valor")
plot(density(test_subset$mejormodelo), main="Distribución de valores predichos", col="blue", xlab="Valor")



