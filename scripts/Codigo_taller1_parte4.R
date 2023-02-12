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
#  Fecha: 08/02/2023 
#
#  Objetivo del script: Este código hace el punto 4 del taller, estudia
#  las brechas salarias con respecto al género. 
#_____________________________________________________________________________#

# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

require(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx)

# - Brecha salarial de género
# - a) Estimación y discusión de la brecha salarial incondicional:


mod1 <- lm(log_salario_m~mujer,GEIH)
output_mod1 <- capture.output(stargazer(mod1, type = "text")) 
write.xlsx(output_mod1, file = "stargazer_output1.xlsx")


# - b) ¿Salario igual para trabajos iguales?
# - Estimación de una brecha de ingresos condicional incorporando variables de 
# - control tales como características similares del trabajador y del puesto.
# - i) Haciendo uso del Teorema FWL:

mod2 <- lm(log_salario_m~mujer + edad + edad_2 + superior + horas_trab_usual + informal+ factor(oficio), GEIH)
output_mod2 <- capture.output(stargazer(mod1, mod2, type = "text", omit = c("oficio"))) 
write.csv(output_mod2, file = "stargazer_output2.csv")

# 1) paso 1
GEIH<-GEIH %>% 
  mutate(y_Resid=lm(log_salario_m ~ edad + edad_2 + superior + horas_trab_usual + informal + factor(oficio),GEIH)$residuals)

# 2) paso 2
GEIH<-GEIH %>% 
  mutate(x_Resid=lm(mujer ~ edad + edad_2+ superior + horas_trab_usual + informal+ + factor(oficio),GEIH)$residuals) 

# 3) Regress the residuals from step 2 on the residuals from step 1

reg2<-lm(y_Resid~x_Resid,GEIH)
stargazer(mod2,reg2,type="text", omit = c("oficio")) 


# - ii) FWL con Bootstrap
# - crear la funcion de FWL
fwl_in_action<-function(data,index) {
  #FWL is the regression of residuals on residuals
  data$y_resid<-resid(lm(log_salario_m ~ edad + edad_2+ superior + horas_trab_usual + informal + factor(oficio), data=data, subset=index))
  data$x_resid<-resid(lm(mujer ~ edad + edad_2+ superior + horas_trab_usual + informal + factor(oficio), data=data, subset=index))
  coef_interest<-coef(lm(y_resid~x_resid, data=data, subset=index))
  coef_interest
}

# - verificar que funciona
lm(log_salario_m~mujer + edad + edad_2 + superior + horas_trab_usual + informal + factor(oficio),GEIH)
fwl_in_action(GEIH,1:nrow(GEIH))

# - implemento Bootstrap
boot(GEIH, fwl_in_action, R = 1000)

# - Gráfica de la brecha edad salario pronosticada con sus edades pico por género:

mod3 <- lm(log_salario_m~mujer + mujer*edad + mujer*edad_2 + edad + edad_2 + superior + horas_trab_usual + informal, GEIH)
results_table <- capture.output(stargazer(mod3, type = "text"))
# - Exportar la tabla a un archivo de Excel
wb <- createWorkbook()
addWorksheet(wb, "Resultados del modelo")
writeData(wb, "Resultados del modelo", results_table)
saveWorkbook(wb, "resultados_modelo.xlsx", overwrite = TRUE)

# - Bootstrap para los obtener errores estándar

base_m <- GEIH[GEIH$mujer == 1, ]

base_h <- GEIH[GEIH$mujer == 0, ]

eta_fn_m<-function(base_m,index){
  coef(lm(log_salario_m~ edad + edad_2 + superior + horas_trab_usual + informal, data = base_m, subset = index)) 
}

eta_fn_h<-function(base_h,index){
  coef(lm(log_salario_m~ edad + edad_2 + superior + horas_trab_usual + informal, data = base_h, subset = index)) 
}



boot_m <- boot(base_m, eta_fn_m, R = 1000)
boot_m

boot_h <- boot(base_h, eta_fn_h, R = 1000)
boot_h

# - Edad pico para mujeres
b1 <- apply(boot_h$t, 2)


base_m$prediccion <- predict(mod3, newdata = x_mujer)
base_m[which.max(base_m$prediccion),][6]

# - Edad pico para hombres 
base_h$prediccion <- predict(mod3, newdata = x_hombre)
base_h[which.max(base_h$prediccion),][6]

# Intervalos de confianza
# - Mujeres 

# - Hombres 





GEIH <- rbind(base_m,base_h)

ggplot() + 
  geom_line(aes(x=base_full$edad,y=predict(mod3, newdata = base_full), color=as.factor(base_full$mujer))) + 
  xlab("Edad") + ylab("Salario Predicho") + ggtitle("Gráfico de Edad versus Salario Predicho")




