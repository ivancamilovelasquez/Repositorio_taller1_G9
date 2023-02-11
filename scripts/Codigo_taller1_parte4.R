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
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot)

# - Brecha salarial de género
# - a) Estimación y discusión de la brecha salarial incondicional:


mod1 <- lm(log_salario_m~mujer,GEIH)
stargazer(mod1, type = "text")


# - b) ¿Salario igual para trabajos iguales?
# - Estimación de una brecha de ingresos condicional incorporando variables de 
# - control tales como características similares del trabajador y del puesto.
# - i) Haciendo uso del Teorema FWL:

mod2 <- lm(log_salario_m~mujer + edad + edad_2 + superior + horas_trab_usual + informal+ factor(oficio), GEIH)
stargazer(mod1, mod2, type = "text", omit = c("oficio"))


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



ggplot() + 
  geom_line(aes(x=GEIH$edad,y=predict(mod2, newdata = GEIH) , color=(as.factor(GEIH$mujer))))
  
ggplot(data = GEIH, aes(x = GEIH$edad, y = predict(mod2, newdata = GEIH))) + 
  geom_point() + xlab("Edad") + ylab("Salario Predicho") + ggtitle("Gráfico de Edad versus Salario Predicho")


GEIH$prediccion <- predict(mod1, newdata = GEIH)
GEIH[which.max(GEIH$prediccion),][5]



modpredict <- lm(GEIH$log_salario_m~as.factor(GEIH$mujer) + GEIH$edad + GEIH$edad_2)



