library(shinydashboard)
library(stringr)
library(shinyWidgets)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(lubridate)
library(formattable)
library(tidyr)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)

# because lol security ------------------------------------------------------------------------------------------------------------------------------------

nube_cuervo_usuarios <- read_csv('datos/contrasenas.csv')


# archivos donde se encuentran las funciones (idealmente en el server sólo deben existir llamadas a funciones) --------------------------------------------

# source('proceso.R', encoding = "UTF-8")
# source('grafica_fill_rate.R', encoding = "UTF-8")
# source('mapa.R', encoding = "UTF-8")
source('funciones_de_carga.R', encoding = "UTF-8")
source('funciones_graficas.R', encoding = "UTF-8")
source('funciones_interfaz.R', encoding = "UTF-8")
source('funciones_extra.R', encoding = "UTF-8")

# inicialización de los parámetros que se leen del excel llamado "parámetros" ------------------------------------------------------------------------------

excel_parametros <- read_excel('datos/setup/parametros.xlsx') %>% data.frame

for(i in 1:nrow(excel_parametros)){
  for(j in 1:length(excel_parametros)){
    excel_parametros[i,j] <- str_replace_all(excel_parametros[i,j],' ','_')
  }
}

# lectura de las matrices de procesos y flag para ver si se incluyen o no --------------------------------------------------------

excel_domestico_procesos_tabla <- read_excel('datos/setup/matriz_procesos.xlsx',sheet = 'domestico') %>% data.frame
excel_domestico_procesos_incluir <- FALSE
if(excel_domestico_procesos_tabla$activar[1] == 'si'){
  excel_domestico_procesos_incluir <- TRUE
}

excel_usa_procesos_tabla <- read_excel('datos/setup/matriz_procesos.xlsx',sheet = 'usa') %>% data.frame
excel_usa_procesos_incluir <- FALSE
if(excel_usa_procesos_tabla$activar[1] == 'si'){
  excel_usadomestico_procesos_incluir <- TRUE
}

excel_row_procesos_tabla <- read_excel('datos/setup/matriz_procesos.xlsx',sheet = 'row') %>% data.frame
excel_row_procesos_incluir <- FALSE
if(excel_row_procesos_tabla$activar[1] == 'si'){
  excel_row_procesos_incluir <- TRUE
}




