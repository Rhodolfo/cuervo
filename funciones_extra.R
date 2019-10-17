
# función para crear la variable f_region------------------------------------------------------------------------------------------------------------------------------------------------------------------

funcion_asigna_region <- function(p_region){
  f_region <- NULL
  
  if(p_region == 'USA')f_region <- 'usa'
  if(p_region == 'Resto del mundo')f_region <- 'row'
  if(p_region == 'Doméstico')f_region <- 'domestico'
  
  return(f_region)
}

# función para crear la variable variables auxiliares de acuerdo a la región ------------------------------------------------------------------------------------------------------------------------------------------------------------------

funcion_asigna_region_variables <- function(p_region,p_parametros,p_input){
  f_region <- NULL
  
  if(p_region == 'USA')f_region <- 'usa'
  if(p_region == 'Resto del mundo')f_region <- 'row'
  if(p_region == 'Doméstico')f_region <- 'domestico'
  
  f_filtros <- eval(parse(text = paste0(
    'p_parametros$',f_region,'_filtros'
  )))
  f_fecha_fin <- eval(parse(text = paste0(
    'p_parametros$',f_region,'_fecha_fin'
  )))
  f_fechas <- eval(parse(text = paste0(
    'p_parametros$',f_region,'_fechas'
  )))
  f_filtros_contenido <- list()
  for(i in 1:length(f_filtros)){
    f_filtros_contenido[[i]] <- eval(parse(text = paste0('as.character(p_input$input_filtro',i,')')))
    f_filtros_contenido[[i]] <- paste0(f_filtros_contenido[[i]],collapse ='","')
  }
  f_cantidad <- eval(parse(text = paste0(      # variable cantidad pedido
    'p_parametros$',f_region,'_cantidades[1]'
  )))
  f_pedido <- eval(parse(text = paste0(       # variable pedido
    'p_parametros$',f_region,'_pedido[1]'
  )))
  f_variables_cantidades <- eval(parse(text = paste0(       # variable pedido
    'p_parametros$',f_region,'_cantidades'
  )))
  f_benchmark <- eval(parse(text = paste0(       # variable pedido
    'p_parametros$',f_region,'_fechas_benchmark'
  )))
  f_cantidad_benchmark <- eval(parse(text = paste0(       # variable pedido
    'p_parametros$',f_region,'_cantidad_benchmark'
  )))
  
  f_resultado <- list()
  f_resultado$region <- f_region
  f_resultado$filtros <- f_filtros
  f_resultado$fecha_fin <- f_fecha_fin
  f_resultado$filtros_contenido <- f_filtros_contenido
  f_resultado$cantidad <- f_cantidad
  f_resultado$pedido <- f_pedido
  f_resultado$fecha_benchmark <- f_benchmark
  f_resultado$fechas <- f_fechas
  f_resultado$variables_cantidades <- f_variables_cantidades
  f_resultado$cantidad_benchmark <- f_cantidad_benchmark
  
  return(f_resultado)
}


# función de filtro vista ejecutiva -------------------------------------------------------------------------------------------------


funcion_filtro_vista_ejecutiva <- function(tablas,p_resultado,input){
  funcion1 <- paste0('tablas$',p_resultado$region,' ')
  funcion2 <- paste0('%>% dplyr::filter(',p_resultado$filtros,' %in% c("',p_resultado$filtros_contenido,'")) ' ,collapse = ' ')
  funcion3 <- paste0(
    '%>% dplyr::filter(!is.na(',input$filtro_fecha_variable,'))'
  )
  funcion4 <- paste0(
    '%>% dplyr::filter(',input$filtro_fecha_variable,' >= "', input$filtro_fecha_rango[1],'") '
  )
  funcion5 <- paste0(
    '%>% dplyr::filter(',input$filtro_fecha_variable,' <= "', input$filtro_fecha_rango[2],'")'
  )
  f_resultado <- eval(parse(text = paste0(funcion1, funcion2, funcion3, funcion4, funcion5)))
  
  return(f_resultado)
}

# función para crear las variables necesarias en el subconjunto de la vista ejecutiva --------------------------------------------

funcion_variables_tabla_subconjunto_vista_ejecutiva <- function(p_tabla,p_region){
  
  eval(parse(text = paste0(     # variable
    'p_tabla <- p_tabla %>%
    mutate(
    ontime = (',p_region$fecha_fin,' <= ',p_region$fecha_benchmark,'),
    ontime = ifelse(ontime %in% c(TRUE, FALSE), ontime, FALSE),
    )'
    )))

  # eval(parse(text = paste0(
  #   'p_tabla <- p_tabla %>%
  #   mutate(
  #   fill_rate = (',p_resultado$fecha_fin,' <= ',p_resultado$fecha_benchmark,')
  #   )'
  #   )))
  
  return(p_tabla)
}

# función para calcular las variables extras en la vista ejecutiva -----------------------------------------------------------------

funcion_variables_extra_vista_ejecutiva <- function(p_tabla,p_tabla_comprimida, input,p_region){
  
  f_beforetime <- 'no aplica'
  f_beforetime_color <- 'blue'
  f_beforetime_icono <- 'kiwi-bird'
  
  f_fillrate <- 'no aplica'
  f_fillrate_color <- 'blue'
  f_fillrate_icono <- 'kiwi-bird'
  
  
  if(nrow(p_tabla_comprimida) > 0){                                           # on time
    f_beforetime <- sum(p_tabla_comprimida$ontime)/nrow(p_tabla_comprimida)
    
    f_beforetime_color <- 'red'
    f_beforetime_icono <- 'frown'
    if(f_beforetime > .75){
      f_beforetime_icono <- 'grin-beam-sweat'
      f_beforetime_color <- 'yellow'
    }
    if(f_beforetime > .85){
      f_beforetime_icono <- 'grin'
      f_beforetime_color <- 'green'
    }
    f_beforetime <- paste0(round(f_beforetime*100,1),'%')
    
    
    if(p_region$cantidad_benchmark != p_region$variables_cantidades[1]){
      eval(parse(text = paste0(
        'f_fillrate <- sum(p_tabla_comprimida$',p_region$cantidad_benchmark,', na.rm = T) / sum(p_tabla_comprimida$',p_region$variables_cantidades[1],', na.rm = T)'
      )))
      
      f_fillrate_color <- 'red'
      f_fillrate_icono <- 'frown'
      if(f_fillrate > .75){
        f_fillrate_icono <- 'grin-beam-sweat'
        f_fillrate_color <- 'yellow'
      }
      if(f_fillrate > .85){
        f_fillrate_icono <- 'grin'
        f_fillrate_color <- 'green'
      }
      f_fillrate <- paste0(round(f_fillrate*100,1),'%')
      
    }
  }

  
  f_resultado <- list()
  f_resultado$beforetime <- f_beforetime
  f_resultado$beforetime_color <- f_beforetime_color
  f_resultado$beforetime_icono <- f_beforetime_icono
  
  f_resultado$fillrate <- f_fillrate
  f_resultado$fillrate_color <- f_fillrate_color
  f_resultado$fillrate_icono <- f_fillrate_icono
  
  return(f_resultado)
  
}








