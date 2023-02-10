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

#Estimación de los betas
reg<- lm(log_salario_m~edad + edad_2,   data = GEIH)
stargazer(reg,type="text") #Regression table

#Bootstrap para los obtener errores estándar
eta_fn<-function(data,index){
  coef(lm(log_salario_m~edad + edad_2, data = GEIH, subset = index)) 
}

eta_fn(GEIH,1:nrow(GEIH))

boot(GEIH, eta_fn, R = 1000)

lm_summary <- summary(reg)$coefficients

#Construir los intervalos de confianza
coefs = data.frame(
  Features = rownames(lm_summary),
  Estimate = lm_summary[,'Estimate'],
  std_error = apply(boot$t,2,sd)
)

alpha = 0.05 # 95% Confidence Interval
coefs$lower = coefs$Estimate - qnorm(alpha/2) * coefs$std_error
coefs$upper = coefs$Estimate + qnorm(alpha/2) * coefs$std_error
coefs = coefs[!(coefs$Features == '(Intercept)'),]


#Plot de los Intervalos de confianza
ggplot(coefs) 
  geom_vline(xintercept = 0, linetype = 4) + #adds a vertical line at zero
  geom_point(aes(x = Estimate, y = Features)) + #point estimate
  geom_segment(aes(y = Features, yend = Features, x = lower, xend = upper),
               arrow = arrow(angle = 90, ends = 'both', 
                             length = unit(0.1, 'cm'))) + #segment representing the CI
  labs(x = 'Coeffienient estimate') +
  theme_bw() 

#Peak age
ggplot() + 
  geom_line(aes(x=GEIH$edad,y=predict(reg, newdata = GEIH)))

GEIH$prediccion <- predict(reg, newdata = GEIH)
GEIH[which.max(GEIH$prediccion),][5]
