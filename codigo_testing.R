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





parametros <- list()

parametros$fecha_actual = as.Date('2019-08-03')   # fecha

parametros$status_morado = -1
parametros$status_rojo = 0
parametros$status_amarillo = 2

parametros$domestico_procesos_tabla = excel_domestico_procesos_tabla        # tablas de procesos
parametros$domestico_procesos_incluir = excel_domestico_procesos_incluir

parametros$usa_procesos_tabla = excel_usa_procesos_tabla
parametros$usa_procesos_incluir = excel_usa_procesos_incluir

parametros$row_procesos_tabla = excel_row_procesos_tabla
parametros$row_procesos_incluir = excel_row_procesos_incluir

parametros$domestico_fechas = excel_parametros %>%        #fechas
  dplyr::filter(!is.na(domestico_fechas)) %>%
  dplyr::select(domestico_fechas) %>%
  unlist %>%
  as.character

parametros$usa_fechas = excel_parametros %>% 
  dplyr::filter(!is.na(usa_fechas)) %>%
  dplyr::select(usa_fechas) %>%
  unlist %>%
  as.character

parametros$row_fechas = excel_parametros %>% 
  dplyr::filter(!is.na(row_fechas)) %>%
  dplyr::select(row_fechas) %>%
  unlist %>%
  as.character

parametros$domestico_cantidades = excel_parametros %>%     # cantidades
  dplyr::filter(!is.na(domestico_cantidades)) %>%
  dplyr::select(domestico_cantidades) %>%
  unlist %>%
  as.character

parametros$usa_cantidades = excel_parametros %>% 
  dplyr::filter(!is.na(usa_cantidades)) %>%
  dplyr::select(usa_cantidades) %>%
  unlist %>%
  as.character

parametros$row_cantidades = excel_parametros %>% 
  dplyr::filter(!is.na(row_cantidades)) %>%
  dplyr::select(row_cantidades) %>%
  unlist %>%
  as.character

parametros$domestico_filtros = excel_parametros %>%         # filtros
  dplyr::filter(!is.na(domestico_filtros)) %>%
  dplyr::select(domestico_filtros) %>%
  unlist %>%
  as.character

parametros$usa_filtros = excel_parametros %>% 
  dplyr::filter(!is.na(usa_filtros)) %>%
  dplyr::select(usa_filtros) %>%
  unlist %>%
  as.character

parametros$row_filtros = excel_parametros %>% 
  dplyr::filter(!is.na(row_filtros)) %>%
  dplyr::select(row_filtros) %>%
  unlist %>%
  as.character

parametros$usa_carpeta = excel_parametros %>%             # carpeta
  dplyr::filter(!is.na(usa_carpeta)) %>%
  dplyr::select(usa_carpeta) %>%
  unlist %>%
  as.character

parametros$row_carpeta = excel_parametros %>% 
  dplyr::filter(!is.na(row_carpeta)) %>%
  dplyr::select(row_carpeta) %>%
  unlist %>%
  as.character

parametros$domestico_carpeta = excel_parametros %>% 
  dplyr::filter(!is.na(domestico_carpeta)) %>%
  dplyr::select(domestico_carpeta) %>%
  unlist %>%
  as.character

parametros$usa_pedido = excel_parametros %>%                # pedido
  dplyr::filter(!is.na(usa_pedido)) %>%
  dplyr::select(usa_pedido) %>%
  unlist %>%
  as.character

parametros$row_pedido = excel_parametros %>% 
  dplyr::filter(!is.na(row_pedido)) %>%
  dplyr::select(row_pedido) %>%
  unlist %>%
  as.character

parametros$domestico_pedido = excel_parametros %>% 
  dplyr::filter(!is.na(domestico_pedido)) %>%
  dplyr::select(domestico_pedido) %>%
  unlist %>%
  as.character

parametros$usa_fecha_inicio = excel_parametros %>%         # fecha inicio
  dplyr::filter(!is.na(usa_fechas)) %>%
  dplyr::select(usa_fechas) %>%
  unlist %>%
  as.character %>%
  head(.,n = 1)

parametros$row_fecha_inicio = excel_parametros %>% 
  dplyr::filter(!is.na(row_fechas)) %>%
  dplyr::select(row_fechas) %>%
  unlist %>%
  as.character %>%
  head(.,n = 1)


parametros$domestico_fecha_inicio = excel_parametros %>% 
  dplyr::filter(!is.na(row_fechas)) %>%
  dplyr::select(row_fechas) %>%
  unlist %>%
  as.character %>%
  head(.,n = 1)

parametros$usa_fecha_fin = excel_parametros %>%     # fecha fin
  dplyr::filter(!is.na(usa_fin)) %>%
  dplyr::select(usa_fin) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$row_fecha_fin = excel_parametros %>% 
  dplyr::filter(!is.na(row_fin)) %>%
  dplyr::select(row_fin) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$domestico_fecha_fin = excel_parametros %>% 
  dplyr::filter(!is.na(domestico_fin)) %>%
  dplyr::select(domestico_fin) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$usa_cantidad_inicio = excel_parametros %>%         # cantidades inicio
  dplyr::filter(!is.na(usa_cantidades)) %>%
  dplyr::select(usa_cantidades) %>%
  unlist %>%
  as.character %>%
  head(.,n = 1)

parametros$row_cantidades_inicio = excel_parametros %>% 
  dplyr::filter(!is.na(row_cantidades)) %>%
  dplyr::select(row_cantidades) %>%
  unlist %>%
  as.character %>%
  head(.,n = 1)

parametros$domestico_cantidades_inicio = excel_parametros %>% 
  dplyr::filter(!is.na(row_cantidades)) %>%
  dplyr::select(row_cantidades) %>%
  unlist %>%
  as.character %>%
  head(.,n = 1)

parametros$usa_cantidades_cerrado = excel_parametros %>%     # cantidades cerrado
  dplyr::filter(!is.na(usa_cantidades)) %>%
  dplyr::select(usa_cantidades) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$row_cantidades_cerrado = excel_parametros %>% 
  dplyr::filter(!is.na(row_cantidades)) %>%
  dplyr::select(row_cantidades) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$domestico_cantidades_cerrado = excel_parametros %>% 
  dplyr::filter(!is.na(domestico_cantidades)) %>%
  dplyr::select(domestico_cantidades) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$usa_fechas_benchmark = excel_parametros %>%
  dplyr::filter(!is.na(usa_fechas_benchmark)) %>%
  dplyr::select(usa_fechas_benchmark) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$row_fechas_benchmark = excel_parametros %>%
  dplyr::filter(!is.na(row_fechas_benchmark)) %>%
  dplyr::select(row_fechas_benchmark) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$domestico_fechas_benchmark = excel_parametros %>%
  dplyr::filter(!is.na(domestico_fechas_benchmark)) %>%
  dplyr::select(domestico_fechas_benchmark) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$usa_cantidad_benchmark = excel_parametros %>%                    # cantidad benchmarck
  dplyr::filter(!is.na(usa_cantidad_benchmark)) %>%
  dplyr::select(usa_cantidad_benchmark) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$row_cantidad_benchmark = excel_parametros %>%
  dplyr::filter(!is.na(row_cantidad_benchmark)) %>%
  dplyr::select(row_cantidad_benchmark) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

parametros$domestico_cantidad_benchmark = excel_parametros %>%
  dplyr::filter(!is.na(domestico_cantidad_benchmark)) %>%
  dplyr::select(domestico_cantidad_benchmark) %>%
  unlist %>%
  as.character %>%
  tail(.,n = 1)

tablas <- list()

tablas$usa <- funcion_cargar_datos(parametros$usa_carpeta,parametros$usa_fechas,parametros$usa_cantidades,parametros$usa_filtros,parametros$usa_pedido, parametros$usa_fechas_benchmark) %>%
  dplyr::filter(Zona_de_ventas != 'ninguno') %>%
  dplyr::filter(Nombre_Región == 'USA')

tablas$row <- funcion_cargar_datos(parametros$row_carpeta,parametros$row_fechas,parametros$row_cantidades,parametros$row_filtros,parametros$row_pedido, parametros$row_fechas_benchmark) %>%
  dplyr::filter(Zona_de_ventas != 'ninguno') %>%
  dplyr::filter(Nombre_Región != 'USA')

tablas$domestico <- funcion_cargar_datos(parametros$domestico_carpeta,parametros$domestico_fechas,parametros$domestico_cantidades,parametros$domestico_filtros,parametros$domestico_pedido, parametros$domestico_fechas_benchmark,parametros$domestico_procesos_incluir)


if(str_detect(excel_parametros$domestico_benchmark_formula[1],'formula')){   # viendo el pedo de una variable custom
  parametros$domestico_fechas_benchmark <- 'fecha_dom_bench_custom'
  eval(parse(text = paste0(
    'tablas$domestico <- tablas$domestico %>%
    mutate(
    fecha_dom_bench_custom = ',parametros$domestico_fechas[1],' + 5
    )'
      )))
}
