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
library(pacman)
p_load(stargazer,boot, bootstrap, ggplot2)

# Punto 3: Age-wage profile

# Estimación de los betas
reg<- lm(log_salario_m~edad + edad_2,   data = GEIH)
stargazer(reg,type="text", align = TRUE, 
              no.space = TRUE, title = "Estimación Salario - Edad",
              out = "C:/Users/andre/OneDrive/Documentos/reg1.tex") 

library(readxl)
GEIH <- read_excel("C:/Users/jorge/Desktop/Problem Set 1/GEIHDATA.xlsx")

# Bootstrap para los obtener errores estándar
eta_fn<-function(data,index){
  coef(lm(log_salario_m~edad + edad_2, data = GEIH, subset = index)) 
}

eta_fn(GEIH,1:nrow(GEIH))

boot <- boot(GEIH, eta_fn, R = 1000)
coef_boot <- boot$t0
SE <- apply(boot$t,2,sd)

# Crear un dataframe con las x y las y

x <- seq(18, 90, length.out = 100)
y <- coef_boot[1] + coef_boot[2] * x + coef_boot[3] * x^2
y_i <- (coef_boot[1]-1.96*SE[1]) + (coef_boot[2]-1.96*SE[2])*x + 
       (coef_boot[3]-1.96*SE[3])*x^2
y_s <- (coef_boot[1]+1.96*SE[1]) + (coef_boot[2]+1.96*SE[2])*x + 
       (coef_boot[3]+1.96*SE[3])*x^2

df <- data.frame(x, y, y_i, y_s)

# Graficar la función

p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(color = "Estimado"), size = 1) +
  geom_line(aes(x = x, y = y_i, color = "Límite inferior"), linetype = "dotted", size = 1) +
  geom_line(aes(x = x, y = y_s, color = "Límite superior"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "", values = c("Estimado" = "blue", "Límite inferior" = "red", "Límite superior" = "red")) +
  labs(x = "Edad", y = "Log(Salario)") +
  theme_classic() +
  scale_x_continuous(limits = c(18, 90)) +
  geom_vline(xintercept = 43, linetype = "dotted") +
  theme(legend.position = "bottom")


ggsave(p3, filename = "C:/Users/andre/OneDrive/Documentos/punto3.png", height = 5, width = 6)
