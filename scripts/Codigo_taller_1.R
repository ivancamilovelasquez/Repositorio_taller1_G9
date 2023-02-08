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

# - Limpiar el environment y el panel

rm(list = ls())
cat("\014")

# - Librerias

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt)

# - 1. Importar Datos

GEIH <-  data.frame()

for (j in 1:10) {
  url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  pagina <- paste0(url,j,".html")
  Data_GitHub <- read_html(pagina)
  temp <-  Data_GitHub %>% html_table()
  GEIH <- bind_rows(GEIH,temp)
}

# - 2. Limpieza de la base de datos

# - Edad (Sólo mayores de 18 años)

GEIH <- GEIH[GEIH$age >= 18, ]

# - Renombrar variable

GEIH <- rename(GEIH, c("edad" = "age"))
GEIH$edad_2 <- GEIH$edad^2

# - Años de educación

GEIH <- rename(GEIH, c("edad" = "age"))

# - Experiencia potencial

# - Género

GEIH$mujer <- ifelse(GEIH$sex == 0, 1, 0)
GEIH$mujer[GEIH$sex == 1] <- 0

# - Ingreso Laboral por horas

# - Estudiante

GEIH$estudiante <- ifelse(GEIH$p6240 == 3, 1, 0)
GEIH$estudiante[GEIH$p6240 != 3] <- 0
GEIH$estudiante[GEIH$p6240 == "."] <- NA

# - Busca trabajo

GEIH$busca_trabajo <- ifelse(GEIH$p6240 == 2, 1, 0)
GEIH$busca_trabajo[GEIH$p6240 != 2] <- 0
GEIH$busca_trabajo[GEIH$p6240 == "."] <- NA

# - Amo(a) de casa

GEIH$amo_casa <- ifelse(GEIH$p6240 == 4, 1, 0)
GEIH$amo_casa[GEIH$p6240 != 4] <- 0
GEIH$amo_casa[GEIH$p6240 == "."] <- NA

# - Hijos en el hogar

GEIH$hijos_hogar <- ifelse(GEIH$p6050 == 3, 1, 0)
GEIH$hijos_hogar[GEIH$p6050 != 3] <- 0
GEIH$hijos_hogar[GEIH$p6050 == "."] <- NA

# - Primaria

GEIH$primaria <- ifelse(GEIH$p6210 == 1, 1, 0)
GEIH$primaria[GEIH$p6210 == "."] <- NA

# - Secundaria

GEIH$secundaria <- ifelse(GEIH$p6210 == 4, 1, 0)
GEIH$secundaria[GEIH$p6210 == "."] <- NA

# - Media

GEIH$media <- ifelse(GEIH$p6210 == 5, 1, 0)
GEIH$media[GEIH$p6210 == "."] <- NA

# - Superior

GEIH$superior <- ifelse(GEIH$p6210 == 6, 1, 0)
GEIH$superior[GEIH$p6210 == "."] <- NA

# - Superior

GEIH$superior <- ifelse(GEIH$p6210 == 6, 1, 0)
GEIH$superior[GEIH$p6210 == "."] <- NA

# - Salario mensual

GEIH <- rename(GEIH, c("salario_mensual" = "y_ingLab_m"))

# - Mantener Ocupados

GEIH <- GEIH[GEIH$oc == 1, ]

# - Ingreso Total

GEIH <- rename(GEIH, c("ingreso_total" = "ingtot"))

# - Experiencia trabajo actual

GEIH <- rename(GEIH, c("exp_trab_actual" = "p6426"))

# - Estrato

GEIH <- rename(GEIH, c("estrato" = "estrato1"))

# - Cabecera

GEIH$cabecera <- ifelse(GEIH$clase == 1, 1, 0)

# - Horas de trabajo a la semana

GEIH <- rename(GEIH, c("horas_trab_usual" = "hoursWorkUsual"))

# - Logaritmo del Salario

GEIH$log_salario_m <- log(GEIH$salario_mensual)

# - Ciudad

GEIH <- rename(GEIH, c("ciudad" = "dominio"))

# -  2.2 Mantener variables


GEIH <- subset(GEIH, select = c("directorio", "secuencia_p", "orden",
                                "mes", "edad", "edad_2", "mujer", 
                                "estudiante", "busca_trabajo", "amo_casa",
                                "hijos_hogar", "primaria", "secundaria",
                                "media", "superior", "salario_mensual",
                                "ingreso_total", "exp_trab_actual",
                                "estrato", "cabecera", "horas_trab_usual",
                                "oficio", "log_salario_m", "informal",
                                "ciudad"))

# -  2.3 Limpieza de valores faltantes 

# Se eliminan las observaciones que tienen valores faltantes en el
# salario nominal mensual

GEIH <- GEIH %>% filter(!is.na(salario_mensual))

# -  2.4 Tratamiento de valores atípicos

# Hablar de el tratamiento con winsorize

GEIH$salario_mensual <- psych::winsor(GEIH$salario_mensual, trim = 0.01)
GEIH$log_salario_m <- log(GEIH$salario_mensual)

