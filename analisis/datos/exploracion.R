setwd("~/repositorios/cuervo/datos")

# librerías ----------------------------

library(readxl)
library(dplyr)
library(stringr)
library(formattable)
library(ggplot2)
library(reshape2)

 # zsdr141 -----------------------------------------------------------------------------------------------------------

# carga y limpieza de los datos

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
      'Cliente_Destinatario...30',
      'ZnVtaCteDes',
      'PaísCteDes',
      'UM...33',
      'No._antiguo_de_material',
      'Aduana_Origen',
      'Nombre_Aduana...42',
      'Aduana_Destino',
      'Nombre_Aduana...44',
      'Tipo_de_Equipo',
      'Doc._Tranp.',
      'Barco',
      'Espacio(Booking)',
      'UM...71',
      'Nombre_País...73',
      'Cambio',
      'Nuevo',
      'Ped.Prog.',
      'Moneda_del_documento',
      'Status_mov.mcía.',
      'Status_de_factura',
      'Status_global',
      'Datos_pos.picking/almac.',
      'Datos_pos.embalaje',
      'Datos_pos.mov.mcía.'
    ),
    .funs = as.factor
  ) %>%
  mutate_at(                     #numéricas
    .vars = c(
      'Posición',
      'Bot._x_Caja',
      'Alc._Vol',
      'LTPlan'
    ),
    .funs = as.numeric
  ) %>%
  mutate_at(                   #fechas
    .vars = c(
      'Fecha_Carga',
      'Fecha_Disponibilidad',
      'Fecha_Pedido',
      'Fecha_Captura_SAP',
      'Fec._Entrega_a_Cte.',
      'Fecha_Entrega',
      'Fecha_Planeación_Transporte',
      'Fecha_Fact.',
      'Fecha_cruce',
      'Fecha_de_Programación',
      'Fec._Entr._PT',
      'Lib.Calidad',
      'Fe_Imp.Fact.',
      'Fe_Imp.Trans',
      'Fecha_Rec.BL',
      'Fe.Guia_Fedex',
      'Fe.Orig.Pref'
    ),
    .funs = as.Date
  ) %>%                         # transformaciones manuales
  mutate(
    ETD = str_replace(ETD,'\\.19','\\.2019'),
    ETD = str_replace_all(ETD,'\\.','\\/'),
    ETD = as.Date(ETD, "%d/%m/%Y"),
    ETA = str_replace(ETA,'\\.19','\\.2019'),
    ETA = str_replace_all(ETA,'\\.','\\/'),
    ETA = as.Date(ETA, "%d/%m/%Y"),
    Hra_Imp.Fact. = as.numeric(format(strptime(Hra_Imp.Fact.,"%d-%m-%Y %H:%M:%S"),'%H%M')),
    Hra_Imp.Trans = as.numeric(format(strptime(Hra_Imp.Trans,"%d-%m-%Y %H:%M:%S"),'%H%M'))
  ) %>%
  mutate(                             # creamos las variables de existencia de las fechas
    ex_Fecha_Carga = !is.na(Fecha_Carga),
    ex_Fecha_Disponibilidad = !is.na(Fecha_Disponibilidad),
    ex_Fecha_Pedido = !is.na(Fecha_Pedido),
    ex_Fecha_Captura_SAP = !is.na(Fecha_Captura_SAP),
    ex_Fecha_Entrega_Cliente = !is.na(Fec._Entrega_a_Cte.),
    ex_Fecha_Entrega = !is.na(Fecha_Entrega),
    ex_Fecha_Planeacion_Transporte = !is.na(Fecha_Planeación_Transporte),
    ex_Fecha_Fact = !is.na(Fecha_Fact.),
    ex_Fecha_Cruce = !is.na(Fecha_cruce),
    ex_Fecha_de_Programacion = !is.na(Fecha_de_Programación),
    ex_Fecha_entrada_PT = !is.na(Fec._Entr._PT),
    ex_Lib_Calidad =!is.na(Lib.Calidad),
    ex_Fecha_Impresion_Fact = !is.na(Fe_Imp.Fact.),
    ex_Imp_Trans =!is.na(Fe_Imp.Trans),
    ex_Fecha_BL = !is.na(Fecha_Rec.BL),
    ex_Guia_Fedex = !is.na(Fe.Guia_Fedex),
    ex_Fecha_Original_Preferente = !is.na(Fe.Orig.Pref),
    ex_ETD = !is.na(ETD),
    ex_ETA = !is.na(ETA)
  )

# guardo el archivo

saveRDS(track,'track_agosto.rds')



# revisión de tabla zsdr141 acumulado anual ------------------------------------------------------------

directorio <- list.files("c:/", "carpeta consultas", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
directorio <- directorio[!str_detect(directorio,'lnk')]

archivos <- list.files(directorio)
archivos_zsdr <- archivos[str_detect(archivos,'XLSX')][1]

track <- read_excel(paste0(directorio,'/',archivos_zsdr)) %>%  #carga
  as.data.frame %>%
  setNames(                             #nombres
    .,str_replace_all(names(.),' ','_')
  )

# función paara cargar consultas SAP -----------------------------------------------------------------------------------------

# p_directorio <- directorio        # parámetros para realizar pruebas en la función
# p_archivos <- 'zsdr141'


funcion_lectura_acumula <- function(p_directorio,p_archivos){ # el directorio y la cadena que define al conjunto de archivos de interés
  
  f_archivos <- list.files(p_directorio)
  f_archivos <- archivos[str_detect(f_archivos,p_archivos)]
  f_archivos <- f_archivos[!is.na(f_archivos)]
  
  f_lista <- list()
  for(i in 1:length(f_archivos)){
    f_lista[[i]]  <- read_excel(paste0(p_directorio,'/',f_archivos[i])) %>%  #carga
      as.data.frame %>%
      setNames(                             #nombres
        .,str_replace_all(names(.),' ','_')
      ) %>%
      filter(!is.na(Código_SAP)) %>%
      mutate_at(                      # factores
        .vars = c(
          'Nombre_Región',
          'Código_SAP',
          'Texto_breve_de_material...5',
          'Paletizado',
          'Marcas',
          'TContenedor',
          'País',
          'Denominación...16',
          'Región',
          'Día_9',
          'Cliente...24',
          'Cliente...25',
          'Zona_de_ventas',
          'Denomin.zona_ventas',
          'Denominación...28',
          'Cliente_Destinatario...29',
          'Cliente_Destinatario...30',
          'Denominación...31',
          'Denominación...32',
          'UM',
          'No._antiguo_de_material',
          'Aduana_Origen',
          'Nombre_Aduana...42',
          'Aduana_Destino',
          'Nombre_Aduana...44',
          'Texto_breve_de_material...46',
          'Doc._Tranp.',
          'Barco',
          'Espacio(Booking)',
          'UMO',
          'Nombre_País',
          'Cambio',
          'Nuevo',
          'Denominación...76',
          'Moneda_del_documento',
          'Status_mov.mcía.',
          'Status_de_factura',
          'Status_global',
          'Datos_pos.picking/almac.',
          'Datos_pos.embalaje',
          'Datos_pos.mov.mcía.'
        ),
        .funs = as.factor
      ) %>%
      mutate_at(                     #numéricas
        .vars = c(
          'Posición',
          'Bot._x_Caja',
          'Grad._Alcohólica',
          'LTPlan'
        ),
        .funs = as.numeric
      ) %>%
      mutate_at(                   #fechas
        .vars = c(
          '...21',
          'Fecha_Carga',
          'Fecha_Disponibilidad',
          'Fecha_Pedido',
          'Fecha_Captura_SAP',
          'Fec._Entrega_a_Cte.',
          'Fecha_Entrega',
          'Fecha_Planeación_Transporte',
          'Fecha_factura',
          'Fecha_cruce',
          'Fecha_de_Programación',
          'Fecha_entrega_real',
          'Lib.Calidad',
          'Fe_Imp.Fact.',
          'Fe_Imp.Trans',
          'Fecha_Rec.BL',
          'Fe.Guia_Fedex',
          'Fe.Orig.Pref'
        ),
        .funs = as.Date
      ) %>%                         # transformaciones manuales
      mutate(
        ETD = str_replace(ETD,'\\.19','\\.2019'),
        ETD = str_replace_all(ETD,'\\.','\\/'),
        ETD = as.Date(ETD, "%d/%m/%Y"),
        ETA = str_replace(ETA,'\\.19','\\.2019'),
        ETA = str_replace_all(ETA,'\\.','\\/'),
        ETA = as.Date(ETA, "%d/%m/%Y"),
        Hra_Imp.Fact. = as.numeric(format(strptime(Hra_Imp.Fact.,"%d-%m-%Y %H:%M:%S"),'%H%M')),
        Hra_Imp.Trans = as.numeric(format(strptime(Hra_Imp.Trans,"%d-%m-%Y %H:%M:%S"),'%H%M'))
      ) %>%
      mutate(                             # creamos las variables de existencia de las fechas
        ex_Fecha_Carga = !is.na(Fecha_Carga),
        ex_Fecha_Disponibilidad = !is.na(Fecha_Disponibilidad),
        ex_Fecha_Pedido = !is.na(Fecha_Pedido),
        ex_Fecha_Captura_SAP = !is.na(Fecha_Captura_SAP),
        ex_Fecha_Entrega_Cliente = !is.na(Fec._Entrega_a_Cte.),
        ex_Fecha_Entrega = !is.na(Fecha_Entrega),
        ex_Fecha_Planeacion_Transporte = !is.na(Fecha_Planeación_Transporte),
        ex_Fecha_factura = !is.na(Fecha_factura),
        ex_Fecha_Cruce = !is.na(Fecha_cruce),
        ex_Fecha_de_Programacion = !is.na(Fecha_de_Programación),
        ex_Fecha_entrega_real = !is.na(Fecha_entrega_real),
        ex_Lib_Calidad =!is.na(Lib.Calidad),
        ex_Fecha_Impresion_Fact = !is.na(Fe_Imp.Fact.),
        ex_Imp_Trans =!is.na(Fe_Imp.Trans),
        ex_Fecha_BL = !is.na(Fecha_Rec.BL),
        ex_Guia_Fedex = !is.na(Fe.Guia_Fedex),
        ex_Fecha_Original_Preferente = !is.na(Fe.Orig.Pref),
        ex_ETD = !is.na(ETD),
        ex_ETA = !is.na(ETA)
      )
  }
  
  f_consolidado <- do.call('rbind',f_lista)
  
  return(f_consolidado)
}


