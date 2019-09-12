



# función carga general de datos ---------------------------------------------------

# carga y junta todos los archivos de excel que se encuentren en una misma carpeta

# p_carpeta <- 'zsdr141'
# p_tipo <- 'zsdr141'

funcion_carga_datos <- function(p_carpeta,p_tipo){
  f_archivos <- list.files(paste0('datos/',p_carpeta))
  f_lista <- list()
  
  
  for(i in 1:length(f_archivos)){
    f_lista[[i]] <- read_excel(paste0('datos/',p_carpeta,'/',f_archivos[i]))
    
    if(p_tipo == 'zsdr141'){
      f_lista[[i]] <- funcion_limpieza_zsdr141(f_lista[[i]])
    }
    
    if(p_tipo == 'zsdr159'){
      f_lista[[i]] <- funcion_limpieza_zsdr159(f_lista[[i]])
    }
    
    
  }
  
  
  
  
  f_condensado <- do.call('rbind',f_lista)
  f_condensado <- as.data.frame(f_condensado)
  return(f_condensado)
}

# función para limpiar la tabla zsdr141 -------------------------------

#limpia cualquier tabla zsdr141




# p_tabla <- tablas$zsdr141


# trash -----------------
# p_tabla %>%
#   filter(is.na(Fe.Orig.Pref)) %>%
#   mutate(a = `Código SAP`, a = as.factor(a)) %>%
#   group_by(a) %>%
#   summarise(conteo = n()) %>%
#   as.data.frame
# 
# p_tabla %>%
#   filter(is.na(Fe.Orig.Pref)) %>%
#   filter(`Código SAP` %in% c('XUS891'))

#

funcion_limpieza_zsdr141 <- function(p_tabla){
  
    f_tabla <- p_tabla %>%
    setNames(                             #nombres
      .,stringr::str_replace_all(names(.),' ','_')
    ) %>%
    dplyr::filter(!is.na(Pedido_SAP)) %>%          # quito los agregados
    mutate_at(                   # factores
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
        'NContenedor',
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
        'Cantidad_de_pedido',
        'StockPed_1154',
        'StockPed_1155',
        'StockPed_11X3',
        'Posición',
        'Stock_Almacen',
        'CJ_9L_@40',
        'CJ_9L',
        'Volumen',
        'Bot._x_Caja',
        'Grad._Alcohólica',
        'Ped.Cte.En_Calidad',
        'LTPlan',
        'No._Factura',
        'No._Orden_Prod.',
        'Cantidad_entrada_de_mercancías',
        'Valor_neto',
        'In_control-cal',
        'Entrega_Cliente',
        'StockPed_XPT1',
        'Costes_internos'
        
        
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
      setNames(
        c(
          'region_nombre','pedido_sap','orden_cliente','material_codigo','material_nombre','cantidad_pedido','stock_pedido_1154','stock_pedido_1155','stock_pedido_11x3',
          'posicion','paletizado','fecha_carga','marca','tipo_contenedor','pais_codigo','var16','fecha_disponibilidad','stock_almacen','region_codigo','fecha_pedido','var21','fecha_captura',
          'dia_9','cliente_a','cliente_b','zona_ventas_codigo','zona_ventas_nombre','pais_nombre','cliente_codigo','cliente_nombre','region_nombre_2','pais_nombre_2','unidad_medida',
          'cajas_9l_40','cajas_9l','volumen','botellas_por_caja','graduacion_alcoholica','material_codigo_antiguo','en_calidad','aduana_origen_codigo','aduana_origen_nombre',
          'aduana_destino_codigo','aduana_destino_nombre','nombre_contenedor','tipo_contenedor_2','fecha_entrega_cliente','fecha_entrega','lead_time_plan','fecha_planeacion_transporte',
          'documento_transporte','sol_crt','cer_crt','fecha_estimada_partida','fecha_estimada_llegada','barco','booking','transportista_codigo','transportista_nombre_a','transportista_nombre_b',
          'comentarios','retraso','numero_factura','fecha_factura','fecha_cruce','numero_orden_produccion','fecha_programacion','fecha_entrega_real','cantidad_entrada_mercancias',
          'liberacion_calidad','unidad_medida_2','numero_lote','pais_codigo_2','cambio','nuevo','programado','motivo_retraso_1','observaciones_1','motivo_retraso_2','observaciones_2',
          'motivo_retraso_3','obervaciones_3','motivo_retraso_4','observaciones_4','motivo_retraso_5','observaciones_5',
          'fecha_impresion_factura','hora_impresion_factura','fecha_impresion_transporte','hora_impresion_transporte','grupo_clientes_codigo',
          'grupo_clientes_nombre','fecha_documento_cruce','fecha_guia_fedex','fecha_original_preferente','fedex_awd','valor_neto','moneda_documento','en_control_calidad',
          'entrga_cliente','stock_pred_xpt1','status_mov_mercancia','status_factura','status_global','datos_post_picking_almacen',
          'datos_post_embalaje','datos_post_mov_mercancia','costos_internos'
        )
      ) %>%
      mutate(                             # creamos las variables de existencia de las fechas
        ex_fecha_carga = !is.na(fecha_carga),
        ex_fecha_disponibilidad = !is.na(fecha_disponibilidad),
        ex_fecha_pedido = !is.na(fecha_pedido),
        ex_fecha_captura = !is.na(fecha_captura),
        ex_fecha_entrega_cliente = !is.na(fecha_entrega_cliente),
        ex_fecha_entrega = !is.na(fecha_entrega),
        ex_fecha_planeacion_transporte = !is.na(fecha_planeacion_transporte),
        ex_fecha_factura = !is.na(fecha_factura),
        ex_fecha_cruce = !is.na(fecha_cruce),
        ex_fecha_programacion = !is.na(fecha_programacion),
        ex_fecha_entrega_real = !is.na(fecha_entrega_real),
        ex_liberacion_calidad =!is.na(liberacion_calidad),
        ex_fecha_impresion_factura = !is.na(fecha_impresion_factura),
        ex_fecha_impresion_transporte =!is.na(fecha_impresion_transporte),
        ex_fecha_documento_cruce = !is.na(fecha_documento_cruce),
        ex_fecha_guia_fedex = !is.na(fecha_guia_fedex),
        ex_fecha_original_preferente = !is.na(fecha_original_preferente),
        ex_fecha_estimada_partida = !is.na(fecha_estimada_partida),
        ex_fecha_estimada_llegada = !is.na(fecha_estimada_llegada)
      )
    
    return(f_tabla)
}

# función de limpieza para la tabla zsdr159 ---------------------------------------------

# p_tabla <- read_excel('datos/zsdr159/zsdr159_10_2019_ene_ago.XLSX') %>%
#   setNames(                             #nombres
#     .,str_replace_all(names(.),' ','_')
#   ) %>%
#   data.frame


funcion_limpieza_zsdr159 <- function(p_tabla){
  f_tabla <- p_tabla %>%
    setNames(                             #nombres
      .,str_replace_all(names(.),' ','_')
    ) %>%
    mutate_at(    # factores
      .vars = c(
        'Ofc.Ventas',
        'Oficina_de_Ventas...2',
        'Canal',
        'Oficina_de_Ventas...4',
        'Nombre',
        'Cl.Ped....25',
        'Cl.Ped....26',
        'Pto.exped....28',
        'Pto.exped....29',
        'Gp.Ventas',
        'Vendedor',
        'Zona_vta.',
        'Zona_de_Ventas',
        'MR...34',
        'Motivo_de_Rechazo',
        'Denominación',
        'MR...44',
        'Motivo_de_BO',
        'Gpo.Clientes'
      ),
      .funs = as.factor
    ) %>%
    mutate_at(   # numéricas
      .vars = c(
        
        'Ctd._Ped._Cj._Nat.',
        'Ctd._Ped._Cj._9_Lit',
        'Ctd._a_entregar',
        'Ctd._Entrega_9Lit',
        'BO_Cj._Nat.',
        'BO_Cj._9_Lit',
        'Ctd.Factura',
        'Ctd.Factura_9Lit',
        'Ctd.Dev.',
        'Ctd.Dev._9Lit',
        'Importe_Ped.',
        'Entrega',
        'Transporte',
        'Factura'
      ),
      .funs = as.numeric
    ) %>%
    mutate_at(   # fechas
      .vars = c(
        'Fe.Creación',
        'Fe.Pre.Entr.',
        'Fecha_de_cita',
        'Fecha_Are',
        'Fe.Factura',
        'Acuse'
      ),
      .funs = as.Date
    ) %>%
    mutate(   # ad hoc
      Pedido = as.factor(as.numeric(Pedido)),
      Cliente = as.factor(as.numeric(Cliente)),
      Pos = as.factor(as.numeric(Pos)),
      Material = as.factor(as.numeric(Material))
    ) %>%
    setNames(
      c(
        'oficina_ventas_codigo',
        'oficina_ventas_nombre',
        'canal_codigo',
        'canal_nombre',
        'numero_pedido',
        'pedido_sap',
        'cliente_codigo',
        'cliente_nombre',
        'cajas_pedido_natural',
        'cajas_pedido_9l',
        'cajas_entrega_natural',
        'cajas_entrega_9l',
        'cajas_backorder_natural',
        'cajas_backorder_9l',
        'cajas_facturadas_natural',
        'cajas_facturadas_9l',
        'cajas_devolucion_natural',
        'cajas_devolucion_9l',
        'fecha_creacion',
        'fecha_original_preferente',
        'fecha_cita',
        'fecha_are',
        'fecha_factura',
        'fecha_acuse',
        'pedido_tipo_codigo',
        'pedido_tipo_nombre',
        'posicion',
        'punto_expedicion_codigo',
        'punto_expedicion_nombre',
        'vendedor_codigo',
        'vendedor_nombre',
        'zona_venta_codigo',
        'zona_venta_nombre',
        'motivo_rechazo_codigo',
        'motivo_rechazo_nombre',
        'material_codigo',
        'material_nombre',
        'importe_pedido',
        'entrega',
        'transporte',
        'bloque_factura',
        'bloque_entrega',
        'factura',
        'motivo_backorder_codigo',
        'motivo_backorder_nombre',
        'grupo_cliente'
        
      )
    )
  
  return(f_tabla)
}



# función para cambiar de meses en cadena a meses numéricos -----------------------------

funcion_mes_a_numero <- function(p_mes){
  
  f_numero <- 0
  f_numero <- switch(p_mes,
    enero = 1,
    febrero = 2,
    marzo = 3,
    abril = 4,
    mayo = 5,
    junio = 6,
    julio = 7,
    agosto = 8,
    septiembre = 9,
    octubre = 10,
    noviembre = 11,
    diciembre = 12
  )
  
  return(f_numero)
}

# función para transformar fechas en números -----------------------------------

# transformación de un conjunto de fechas en valores numéricos tomando como base
# una fecha central que tomará valor cero. Todas las demás tendrán el valor
# correspondiente al número de días que transcurren antes o después de dicha fecha

# p_tabla <- tablas$vis
# p_fecha_focal <- 'fecha_pedido'
# p_fechas <- c('fecha_original_preferente','fecha_entrega','fecha_captura','fecha_entrega_cliente')
# 
# funcion_fechas_centradas <- function(p_tabla,p_fecha_focal,p_fechas){
#   f_tabla <- p_tabla %>%
#     select(p_fechas,p_fecha_focal)
#   
#   f_funcion_1 <- 'f_tabla <- f_tabla %>% mutate('
#   f_funcion_2 <- paste0(p_fechas,' = ', p_fechas, ' - ', p_fecha_focal)
#   
# }
# 
# 

# función para extraer el año y el mes de una variable en una tabla y con ello crear un catálogo de selección --------------------------------------------

# p_tabla <- tablas$zsdr159

funcion_ano_mes <- function(p_tabla){
    f_tabla <- p_tabla %>%
      mutate(
        ano = year(fecha_original_preferente),
        mes = month(fecha_original_preferente),
        ano_mes = paste0(
          ano,
          '_',
          str_sub(paste0('0',mes),-2)
        )
      )
    return(f_tabla)
}

