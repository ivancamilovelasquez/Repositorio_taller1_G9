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
stargazer(mod1)
# - Exportar la tabla a un archivo de Excel
reg_mod_1 <- createWorkbook()
addWorksheet(reg_mod_1, "Resultados del modelo")
writeData(reg_mod_1, "Resultados del modelo", results_table)
saveWorkbook(reg_mod_1, "resultados_modelo.xlsx", overwrite = TRUE, 
             file = "C:/Users/Santiago/Downloads/Escritorio/DOCUMENTOS SANTIGO/Maestria Uniandes/Big Data & Machine Learning/Repositorios/Repositorio_taller1_G9/views/reg_mod_1.xlsx")


# - b) ¿Salario igual para trabajos iguales?
# - Estimación de una brecha de ingresos condicional incorporando variables de 
# - control tales como características similares del trabajador y del puesto.
# - i) Haciendo uso del Teorema FWL:

GEIH <- GEIH %>% 
        mutate(mujer_edad = mujer*edad,
               mujer_edad2 = mujer*edad_2)

mod2 <- lm(log_salario_m~mujer + edad + edad_2 + superior + horas_trab_usual + informal, GEIH)
stargazer(mod1, mod2, omit = c("oficio"))
# - Exportar la tabla a un archivo de Excel
reg_mod_2 <- createWorkbook()
addWorksheet(reg_mod_2, "Resultados del modelo")
writeData(reg_mod_2, "Resultados del modelo", results_table)
saveWorkbook(reg_mod_2, "resultados_modelo.xlsx", overwrite = TRUE, 
             file = "C:/Users/Santiago/Downloads/Escritorio/DOCUMENTOS SANTIGO/Maestria Uniandes/Big Data & Machine Learning/Repositorios/Repositorio_taller1_G9/views/reg_mod_2.xlsx")

# 1) paso 1
GEIH<-GEIH %>% 
  mutate(y_Resid=lm(log_salario_m ~ edad + edad_2 + superior + horas_trab_usual + informal,GEIH)$residuals)

# 2) paso 2
GEIH<-GEIH %>% 
  mutate(x_Resid=lm(mujer ~ edad + edad_2+ superior + horas_trab_usual + informal,GEIH)$residuals) 

# 3) Regress the residuals from step 2 on the residuals from step 1

reg2<-lm(y_Resid~x_Resid,GEIH)
stargazer(mod2,reg2) 


# - ii) FWL con Bootstrap
# - crear la funcion de FWL
fwl_in_action<-function(GEIH,index) {
  #FWL is the regression of residuals on residuals
  GEIH$y_resid<-resid(lm(log_salario_m ~ edad + edad_2+ superior + horas_trab_usual + informal , data=GEIH, subset=index))
  GEIH$x_resid<-resid(lm(mujer ~ edad + edad_2+ superior + horas_trab_usual + informal, data=GEIH, subset=index))
  coef_interest<-coef(lm(y_resid~x_resid, data=GEIH, subset=index))
  coef_interest
}

# - verificar que funciona
lm(log_salario_m~mujer + edad + edad_2 + superior + horas_trab_usual + informal ,GEIH)
fwl_in_action(GEIH,1:nrow(GEIH))

# - implemento Bootstrap
boot_FWL <- boot(GEIH, fwl_in_action, R = 1000)
boot_FWL



# - Gráfica de la brecha edad salario pronosticada con sus edades pico por género:

mod3 <- lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH)
coefs <- mod3$coef
coefs

eta_mod3_fn_m<-function(GEIH,index){
  
  # Obtener coeficientes
  coefs<-lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH, subset = index)$coefficients
  
  # Colocar coeficientes en escalares 
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5] 
  b5<-coefs[6] 
  b6<-coefs[7]
  b7<-coefs[8] 
  b8<-coefs[9] 
  
  # Calcular edad pico para mujeres
  peak_m<- (-(b2+b7)/(2*(b3+b8)))
  
  return(peak_m)
}

eta_mod3_fn_m(GEIH,1:nrow(GEIH))

boot_m <- boot(GEIH, eta_mod3_fn_m, R = 1000)
boot_m

# - Calculo ntervalos de confianza 

intervalo_m <- quantile(boot_m$t, c(0.025, 0.975))
intervalo_m

eta_mod3_fn_h<-function(GEIH,index){
  
  # Obtener coeficientes
  coefs<-lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH, subset = index)$coefficients
  
  # Colocar coeficientes en escalares 
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5] 
  b5<-coefs[6] 
  b6<-coefs[7]
  b7<-coefs[8] 
  b8<-coefs[9] 
  
  # Calcular edad pico para mujeres
    peak_h <- (-b2/(2*b3))
  
  return(peak_h)
}

eta_mod3_fn_h(GEIH,1:nrow(GEIH))

boot_h <- boot(GEIH, eta_mod3_fn_h, R = 1000)
boot_h

# intervalos de confianza para los hombres 

intervalo_h <- quantile(boot_h$t, c(0.025, 0.975))
intervalo_h
################################################################################

# - Grafica edades pico para mujeres 

edad_m<-function(GEIH,index){
  
  # Obtener coeficientes
  coefs<-lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH, subset = index)$coefficients
  
  # Colocar coeficientes en escalares 
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5] 
  b5<-coefs[6] 
  b6<-coefs[7]
  b7<-coefs[8] 
  b8<-coefs[9] 
  
  # Calcular edad pico para mujeres
  beta_edad_m<- ((b2+b7))
  
  
  return(beta_edad_m)
  
}

edad_m(GEIH,1:nrow(GEIH))

boot_edad_m <- boot(GEIH, edad_m, R = 1000)
boot_edad_m
coef_ed_mujer <- boot_edad_m$t0
se_ed_mujer <- apply(boot_edad_m$t,2,sd)


edad2_m<-function(GEIH,index){
  
  # Obtener coeficientes
  coefs<-lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH, subset = index)$coefficients
  
  # Colocar coeficientes en escalares 
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5] 
  b5<-coefs[6] 
  b6<-coefs[7]
  b7<-coefs[8] 
  b8<-coefs[9] 
  
  # Calcular edad pico para mujeres
  beta_edad2_m <- (b3+b8)
  
  
  return(beta_edad2_m)
  
}

edad2_m(GEIH,1:nrow(GEIH))

boot_edad2_m <- boot(GEIH, edad2_m, R = 1000)
boot_edad2_m
coef_ed2_mujer <- boot_edad2_m$t0
se_ed2_mujer <- apply(boot_edad2_m$t,2,sd)

###############################################################################
###############################################################################

# - Grafica edades pico para hombres

edad_h<-function(GEIH,index){
  
  # Obtener coeficientes
  coefs<-lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH, subset = index)$coefficients
  
  # Colocar coeficientes en escalares 
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5] 
  b5<-coefs[6] 
  b6<-coefs[7]
  b7<-coefs[8] 
  b8<-coefs[9] 
  
  # Calcular edad pico para mujeres
  beta_edad_h<- ((b2))
  
  
  return(beta_edad_h)
  
}

edad_h(GEIH,1:nrow(GEIH))

boot_edad_h <- boot(GEIH, edad_h, R = 1000)
boot_edad_h
coef_ed_hombre <- boot_edad_h$t0
se_ed_hombre <- apply(boot_edad_h$t,2,sd)


edad2_h<-function(GEIH,index){
  
  # Obtener coeficientes
  coefs<-lm(log_salario_m ~ mujer+ edad + edad_2+ superior + horas_trab_usual + informal+ mujer_edad + mujer_edad2,GEIH, subset = index)$coefficients
  
  # Colocar coeficientes en escalares 
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5] 
  b5<-coefs[6] 
  b6<-coefs[7]
  b7<-coefs[8] 
  b8<-coefs[9] 
  
  # Calcular edad pico para mujeres
  beta_edad2_h <- (b3)
  
  
  return(beta_edad2_h)
  
}

edad2_h(GEIH,1:nrow(GEIH))

boot_edad2_h <- boot(GEIH, edad2_h, R = 1000)
boot_edad2_h
coef_ed2_hombre <- boot_edad2_h$t0
se_ed2_hombre <- apply(boot_edad2_h$t,2,sd)
###############################################################################
###############################################################################
# Crear un dataframe con las x y las y

x <- seq(18, 90, length.out = 100)


y_m <- 12.33 + coef_ed_mujer * x + coef_ed2_mujer * x^2
y_m_i <- 12.19 + (coef_ed_mujer-1.96*se_ed_mujer) * x + (coef_ed2_mujer-1.96*se_ed2_mujer) * x^2
y_m_s <- 12.47 + (coef_ed_mujer+1.96*se_ed_mujer) * x + (coef_ed2_mujer+1.96*se_ed2_mujer) * x^2


y_h <- coef_ed_hombre * x + coef_ed2_hombre * x^2
y_h_i <- (coef_ed_hombre-1.96*se_ed_hombre) * x + (coef_ed2_hombre-1.96*se_ed2_hombre) * x^2
y_h_s <- (coef_ed_hombre+1.96*se_ed_hombre) * x + (coef_ed2_hombre+1.96*se_ed2_hombre) * x^2


df <- data.frame(x, y_m, y_m_i, y_m_s,y_h,y_h_i,y_h_s)

# Graficar la función

  ggplot(df, aes(x = x, y = y_m)) +
  geom_line(aes(color = "Estimado"), size = 1) +
  geom_line(aes(x = x, y = y_m_i, color = "Límite inferior"), linetype = "dotted", size = 1) +
  geom_line(aes(x = x, y = y_m_s, color = "Límite superior"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "", values = c("Estimado" = "blue", "Límite inferior" = "red", "Límite superior" = "red")) +
  labs(x = "Edad", y = "Log(Salario)") +
  theme_classic() +
  scale_x_continuous(limits = c(18, 90)) +
  geom_vline(xintercept = 45, linetype = "dotted") +
  theme(legend.position = "bottom")
  
  ggplot(df, aes(x = x, y = y_h)) +
    geom_line(aes(color = "Estimado"), size = 1) +
    geom_line(aes(x = x, y = y_h_i, color = "Límite inferior"), linetype = "dotted", size = 1) +
    geom_line(aes(x = x, y = y_h_s, color = "Límite superior"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "", values = c("Estimado" = "blue", "Límite inferior" = "red", "Límite superior" = "red")) +
    labs(x = "Edad", y = "Log(Salario)") +
    theme_classic() +
    scale_x_continuous(limits = c(18, 90)) +
    geom_vline(xintercept = 49, linetype = "dotted") +
    theme(legend.position = "bottom")


