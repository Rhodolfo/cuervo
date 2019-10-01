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


# source('gs.R', encoding = "UTF-8") #mientras vemos qué pedo

nube_cuervo_usuarios <- read_csv('datos/contrasenas.csv')

# source('proceso.R', encoding = "UTF-8")
# source('grafica_fill_rate.R', encoding = "UTF-8")
# source('mapa.R', encoding = "UTF-8")
source('funciones_de_carga.R', encoding = "UTF-8")
source('funciones_graficas.R', encoding = "UTF-8")
source('funciones_interfaz.R', encoding = "UTF-8")
source('funciones_extra.R', encoding = "UTF-8")

excel_parametros <- read_excel('datos/setup/parametros.xlsx') %>% data.frame

for(i in 1:nrow(excel_parametros)){
  for(j in 1:length(excel_parametros)){
    excel_parametros[i,j] <- str_replace_all(excel_parametros[i,j],' ','_')
  }
}

