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

# - Librerias y paquetes 

require(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot)

# - Brecha salarial de género
# - a) Estimación y discusión de la brecha salarial incondicional:

mod1 <- lm(GEIH$log_salario_m~GEIH$mujer)
stargazer(mod1, type = "text")

view(GEIH$oficio)

# - b) ¿Salario igual para trabajos iguales?
# - Estimación de una brecha de ingresos condicional incorporando variables de 
# - control tales como características similares del trabajador y del puesto.
# - i) Haciendo uso del Teorema FWL:

mod2 <- lm(GEIH$log_salario_m~GEIH$mujer + GEIH$superior + GEIH$horas_trab_usual + GEIH$edad + GEIH$edad_2 + GEIH$informal)
stargazer(mod2, type = "text")


# 1) paso 1
GEIH<-GEIH %>% mutate(weightResidF=lm(log_salario_m ~ superior + horas_trab_usual + GEIH$edad + GEIH$edad_2 + GEIH$informal,GEIH)$residuals) #Residuals of weight~foreign 

# 2) paso 2
GEIH<-GEIH %>% mutate(mpgResidF=lm(mujer ~ superior + horas_trab_usual + GEIH$edad + GEIH$edad_2 + GEIH$informal,GEIH)$residuals) #Residuals of mpg~foreign 

# 3) Regress the residuals from step 2 on the residuals from step 1

reg2<-lm(weightResidF~mpgResidF,GEIH)
stargazer(mod2,reg2,type="text",digits=7) # with stargazer we can visualize the coefficients next to each other

# - ii) FWL con Bootstrap


eta.fn<-function(data,index){
  coef(lm(weightResidF~mpgResidF,GEIH, subset = index))
}

boot(GEIH, eta.fn, R = 1000)

# - Gráfica de la brecha edad salario pronosticada con sus edades pico por género:

ggplot() + 
  geom_line(aes(x=GEIH$edad,y=predict(mod2, newdata = GEIH) , group=(GEIH$mujer)))
  
GEIH$prediccion <- predict(mod1, newdata = GEIH)
GEIH[which.max(GEIH$prediccion),][5]










