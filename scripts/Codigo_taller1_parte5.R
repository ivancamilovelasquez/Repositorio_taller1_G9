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
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, ggthemes, data.table , openxlsx , grid)

# - Fijar el Seed 
set.seed(10101)

# - Punto 5A

# Crear una variable ID para la división de la base: train y test 

GEIH$id <- 1:nrow(GEIH)


# Dividir la base en 70% (train), 30% (test)

train <- GEIH %>%  group_by(oficio) %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(GEIH, train, by = 'id')


# - Punto 5B

# Modelo base solo con la constante 

mod1 <- lm(log_salario_m~1 , data = train)

# Modelos punto 3

mod2 <- lm(log_salario_m~edad + edad_2, data = train)

# Modelos punto 4

mod3 <- lm(log_salario_m~ mujer , data = train )

mod4 <- lm(log_salario_m~mujer + edad + edad_2 + superior + horas_trab_usual 
           + informal+ factor(oficio), data = train)

mod5 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
           + superior + horas_trab_usual + informal + factor(oficio), data = train)


# Nuevos modelos 

mod6 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
           + superior + horas_trab_usual + informal + factor(oficio) + media , data = train)

mod7 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
           + superior + horas_trab_usual + informal + factor(oficio) + media 
           + exp_trab_actual  , data = train)

mod8 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
           + superior + horas_trab_usual + informal + factor(oficio) + media 
           + exp_trab_actual + factor(estrato)  , data = train)

mod9 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
           + superior + horas_trab_usual + informal + factor(oficio) + media 
           + exp_trab_actual + factor(estrato) + I(exp_trab_actual^2) , data = train)

mod10 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
           + superior + horas_trab_usual + informal + factor(oficio) + media 
           + exp_trab_actual + factor(estrato) + I(exp_trab_actual^2) 
           + I(horas_trab_usual^2) , data = train)


# Crear archivo de Excel donde pondremos todas las salidas

punto5_excel <- createWorkbook()
addWorksheet(punto5_excel, "Train")
salidas1 <- as.data.frame(stargazer(mod1, mod2, mod3, mod4 , mod5 , mod6, 
                                    mod7, mod8, mod9, mod10, type = "text" 
                                    , omit = c("oficio") , digits = 3))
writeData(punto5_excel, "Train", salidas1)
saveWorkbook(punto5_excel, file = "D:\\2023\\ANDES\\Big data\\Taller1\\Repositorio_taller1_G9\\views\\Salida_punto5.xlsx", overwrite = TRUE)


# Calcular el MSE en los  train 

MSE_mod_train <- numeric(10) 
for (i in 1:10) {
  modelo_t <- get(paste0("mod", i)) 
  prediction_t <- predict(modelo_t,newdata = train)
  train[[paste0("mod", i)]] <- prediction_t
  MSE_mod_train[i] <- with(train, mean((log_salario_m-prediction_t)^2))
}
which.min(MSE_mod_train)

# Tabla de los MSE de los modelos 

Modelo <- c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5",
            "Modelo 6", "Modelo 7", "Modelo 8", "Modelo 9", "Modelo 10")
MSE_train_data_frame <- data.frame(Modelo, MSE_mod_train)
t(MSE_train_data_frame)
write.xlsx(MSE_train_data_frame, 
           file = "D:/2023/ANDES/Big data/Taller1/Repositorio_taller1_G9/views/MSE_train_data_frame.xlsx")



# Calcular el MSE en el test

MSE_mod <- numeric(10) 
for (i in 1:10) {
  modelo <- get(paste0("mod", i)) 
  prediction <- predict(modelo,newdata = test)
  test[[paste0("mod", i)]] <- prediction
  MSE_mod[i] <- with(test, mean((log_salario_m-prediction)^2))
}
which.min(MSE_mod)

# Tabla de los MSE de los modelos 
MSE_test_data_frame <- data.frame(Modelo, MSE_mod)
t(MSE_test_data_frame)
write.xlsx(MSE_test_data_frame, 
           file = "D:/2023/ANDES/Big data/Taller1/Repositorio_taller1_G9/views/MSE_test_data_frame.xlsx")




# Dado que el mejor modelo es el 10 ,  guardamos la predicción y el verdadero valor 

test$mejormodelo <- predict(mod10 ,newdata = test)
test_subset <- subset(test, select = c("log_salario_m", "mejormodelo"))


# Graficas:  valor verdadero y valor predicho, mejor modelo

par(mfrow = c(1, 1))
plot(density(test_subset$log_salario_m), main = "Distribución de los valores observador y predichos", 
     col = "red", xlab = "Log salario", ylab = "Densidad", xlim = c(min(c(test_subset$log_salario_m, test_subset$mejormodelo))
     , max(c(test_subset$log_salario_m, test_subset$mejormodelo)))) 
  lines(density(test_subset$mejormodelo), col = "blue")
  legend("topright", c("Valor observado", "Valor predicho"), lty = c(1, 1), col = c("red", "blue")) +
  theme(legend.position = "topright", text = element_text(size = 12, family = "Arial")) 
  


# Diferencia entre el valor verdadero y el predicho en el mejor modelo

test_subset$diferencia_absoluta <- abs(test_subset$log_salario_m - test_subset$mejormodelo)
test_subset$log_salario_m_percentil <- 100 * (rank(test_subset$log_salario_m) - 1) / nrow(test_subset)
test_subset_ordenado <- test_subset[order(-test_subset$diferencia), ]
#Tomar las diez diferencias mayores 
top_10 <- head(test_subset_ordenado, 10)
write.xlsx(top_10, 
           file = "D:/2023/ANDES/Big data/Taller1/Repositorio_taller1_G9/views/TOP_10_diferencia_estimacion_mejor_modelo.xlsx")



# 5 B lo hacemos con K folds. Dividimos los datos en 70% train 30% test 
# como inicialmente lo hicimos. Hacemos k folds en el 70% dejando siempre 
# un k como testeo. Luego el mejor modelo lo ponemos a predecir sobre el 
# 30% inicial que se dejó para testo y mirar que tan bueno es el modelo. 

set.seed(10101)
K <- 5
index <- split(1:6920, 1: K)
splt <- lapply(1:K, function(ind) train[index[[ind]], ])
head(splt[1])
m1 <- lapply(1:K, function(ii) lm(log_salario_m~ mujer + superior + horas_trab_usual + edad 
                                  + edad_2 + informal + estrato + cabecera, 
                                  data = rbindlist(splt[-ii])))
#Predicción 
p1 <- lapply(1:K, function(ii) data.frame(predict(m1[[ii]], newdata = rbindlist(splt[ii]))))
for (i in 1:K) {
  colnames(p1[[i]])<-"yhat" 
  splt[[i]] <- cbind(splt[[i]], p1[[i]])
  
}
MSE2_k <- lapply(1:K, function(ii) mean((splt[[ii]]$log_salario_m - splt[[ii]]$yhat)^2))
MSE2_k
mean(unlist(MSE2_k))

# Los cálculos anteriores los hacemos sobre el 30% de test.

K <- 5
index_test <- split(1:592, 1: K)
splt_test <- lapply(1:K, function(ind) test[index_test[[ind]], ])
p1_test <- lapply(1:K, function(ii) data.frame(predict(m1[[ii]], newdata = rbindlist(splt_test[ii]))))
for (i in 1:K) {
  colnames(p1_test[[i]])<-"yhat" 
  splt_test[[i]] <- cbind(splt_test[[i]], p1_test[[i]])
  
}
MSE2_k_test <- lapply(1:K, function(ii) mean((splt_test[[ii]]$log_salario_m - splt_test[[ii]]$yhat)^2))
MSE2_k_test
mean(unlist(MSE2_k_test))


# 5 D LOOCV : Los dos mejores modelos son el 10 y el 9 
set.seed(10101)
K <- 9892
index_LOOCV <- split(1:9892, 1: K)
splt_LOOCV <- lapply(1:K, function(ind) GEIH[index_LOOCV[[ind]], ])
m2 <- lapply(1:K, function(ii) lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 
                                  + superior + horas_trab_usual + informal + factor(oficio) + media 
                                  + exp_trab_actual + factor(estrato) + I(exp_trab_actual^2) 
                                  + I(horas_trab_usual^2),data = rbindlist(splt_LOOCV[-ii])))
#Predicción 
p2 <- lapply(1:K, function(ii) data.frame(predict(m2[[ii]], newdata = rbindlist(splt_LOOCV[ii]))))
for (i in 1:K) {
  colnames(p2[[i]])<-"yhat" 
  splt_LOOCV[[i]] <- cbind(splt_LOOCV[[i]], p2[[i]])
  
}
MSE2_LOOCV <- lapply(1:K, function(ii) mean((splt_LOOCV[[ii]]$log_salario_m - splt_LOOCV[[ii]]$yhat)^2))
MSE2_LOOCV
mean(unlist(MSE2_LOOCV))






