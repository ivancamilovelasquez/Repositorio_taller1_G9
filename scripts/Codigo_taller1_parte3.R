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
#  Fecha: 07/02/2023 

p_load(stargazer,boot)

# Punto 3: Age-wage profile

# Estimación de los betas
reg<- lm(log_salario_m~edad + edad_2,   data = GEIH)
stargazer(reg,type="text", align = TRUE, 
              no.space = TRUE, title = "Estimación Salario - Edad",
              out = "C:/Users/andre/OneDrive/Documentos/reg1.tex") 

# Bootstrap para los obtener errores estándar
eta_fn<-function(data,index){
  coef(lm(log_salario_m~edad + edad_2, data = GEIH, subset = index)) 
}

eta_fn(GEIH,1:nrow(GEIH))

boot <- boot(GEIH, eta_fn, R = 1000)

# Peak age
ggplot() + 
  geom_line(aes(x=GEIH$edad,y=predict(reg, newdata = GEIH)))

GEIH$prediccion <- predict(reg, newdata = GEIH)
GEIH[which.max(GEIH$prediccion),][5]

# Construir los intervalos de confianza
b <- quantile(apply(boot$t,2,sd), c(0.025,0.975))
GEIH$low <- GEIH$prediccion + b[1]
GEIH$up <- GEIH$prediccion + b[2]

# Gráfica del Peak Age
library(ggplot2)
ggplot() +
  geom_line(aes(x=GEIH$edad,y=predict(reg, newdata = GEIH))) +
  geom_point() +
  geom_vline(xintercept = 43) +
  geom_errorbar(aes(x= GEIH$edad, ymin = GEIH$low, ymax = GEIH$up)) +
  theme_test() +
  labs(x = "Edad", y = "Logaritmo del Salario")
