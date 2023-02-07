#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#
#  Fecha: 06/02/2023 

# - Librerias

library(pacman)
p_load(rvest, tidyverse)

# - 1. Importar Datos

GEIH <-  data.frame()

for (j in 1:2) {
  url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  pagina <- paste0(url,j,".html")
  Data_GitHub <- read_html(pagina)
  temp <-  Data_GitHub %>% html_table()
  GEIH <- bind_rows(GEIH,temp)
}

# - 2. Limpieza de la base de datos

# - Edad (Sólo mayores de 18 años)

GEIH <- GEIH[GEIH$age >= 18, ]
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