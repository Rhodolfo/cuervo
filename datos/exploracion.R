setwd("~/repositorios/cuervo/datos")

# librerías ----------------------------

library(readxl)
library(dplyr)
library(stringr)

# carga y limpieza de los datos ---------------

track <- read_excel('consulta_prueba.xlsm',sheet = 'RawData') %>%  #carga
  as.data.frame %>%
  setNames(                             #nombres
    .,str_replace_all(names(.),' ','_')
  ) %>%
  mutate_at(                      # factores
    .vars = c(
      'Nombre_Región',
      'Código_SAP',
      'Producto',
      'Paletizado',
      'Marcas',
      'TContenedor',
      'País',
      'GpoCte',
      'Región',
      'FE_BOOK',
      'Día_9',
      'Cliente...24',
      'Cliente...25',
      'Zona_de_ventas',
      'ZnVtaCteSo',
      'Nombre_País...28',
      'Cliente_Destinatario...29',
      'Cliente_Destinatario...30'
    ),
    .funs = as.factor
  ) %>%
  mutate_at(                     #numéricas
    .vars = c(
      'Posición'
    ),
    .funs = as.numeric
  ) %>%
  mutate_at(
    .vars = c(
      'Fecha_Carga',
      'Fecha_Disponibilidad',
      'Fecha_Pedido',
      'Fecha_Captura_SAP'
    ),
    .funs = as.Date
  )



track %>%
  select(21:30) %>%
  summary

track %>%
  select(21:30) %>%
  str
