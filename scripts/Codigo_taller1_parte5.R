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

# Modelos punto 3
mod1_train <- lm(log_salario_m~edad + edad_2, data = train)

# Modelos punto 4
mod2_train <- lm(log_salario_m~ mujer , data = train )

mod3_train <- lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                 + edad_2 + informal, data = train)

# Nuevos modelos 

mod2_train <- lm(log_salario_m~ mujer , data = train )

mod3_train <- lm(log_salario_m~ mujer + superior + secundaria + horas_trab_usual 
                 + exp_trab_actual + amo_casa + edad + edad_2 + informal, data = train)

mod4_train <- lm(log_salario_m~ mujer + superior + secundaria + horas_trab_usual 
                 + exp_trab_actual + amo_casa + edad + edad_2 + + oficio 
                 + informal, data = train)

stargazer(mod1_train,mod2_train,mod3_train,mod4_train, type = "text")


