

# (main) función para grafica 1 -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- tablas$domestico %>% dplyr::filter(Fecha_Are >= '2019-08-01') %>% dplyr::filter(Fecha_Are <= '2019-08-07')
# p_texto_x <- 'x'
# p_texto_y <- 'y'
# p_variables_fecha <- parametros$domestico_fechas
# p_variable_pedido <- parametros$domestico_pedido[1]
# p_compresion <- TRUE
# p_variables_cantidades <- parametros$domestico_cantidades
# p_fecha_benchmark <- parametros$domestico_fechas_benchmark

funcion_main_grafica_1 <- function(p_tabla, p_compresion = FALSE,p_texto_x,p_texto_y,p_variables_fecha,p_variable_pedido,p_variables_cantidades, p_fecha_benchmark){
  if(p_compresion){
    f_resultado <- funcion_compresion_fecha(p_tabla,p_variables_fecha ,p_variable_pedido,p_variables_cantidades,p_fecha_benchmark)
    f_tabla <- f_resultado$tabla
    f_variables_fecha <- f_resultado$fechas
    f_variables_benchmark <- f_resultado$fechas_benchmark
  }
  f_resultado <- funcion_solo_variables_maximo(f_tabla,c(f_variables_fecha,f_variables_benchmark))
  f_tabla <- f_resultado$tabla
  f_variables_fecha <- f_resultado$variables[-length(f_resultado$variables)]
  f_variables_benchmark <- f_resultado$variables[length(f_resultado$variables)]
  f_resultado <- funcion_ordena_fechas(f_tabla,f_variables_fecha)
  f_tabla <- f_resultado$tabla
  f_variables_fecha <- f_resultado$fechas
  f_grafica <- funcion_grafica_pedidos_puntos(f_tabla, f_variables_fecha,f_variables_benchmark)
  return(f_grafica)
}

 # funcion_main_grafica_1(p_tabla, p_compresion = TRUE,p_texto_x,p_texto_y,p_variables_fecha,p_variable_pedido,p_variables_cantidades, p_fecha_benchmark)

# (main) función para grafica 2 -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- tablas$domestico %>% dplyr::filter(Fecha_Are >= '2019-08-01') %>% dplyr::filter(Fecha_Are <= '2019-08-07')
# p_texto_x <- 'prceso'
# p_texto_y <- 'cantidad'
# p_variables_fecha <- parametros$domestico_fechas
# p_variable_pedido <- parametros$domestico_pedido[1]
# p_compresion <- TRUE
# p_variables_cantidades <- parametros$domestico_cantidades
# p_texto_label <- 'entregas'
# p_tipo_fgb <- 'cuantos'
# p_fecha_benchmark <- parametros$domestico_fechas_benchmark


funcion_main_grafica_2 <- function(p_tabla, p_compresion = FALSE, p_texto_x, p_texto_y, p_variables_fecha, p_variable_pedido, p_variables_cantidades, p_texto_label, p_tipo_fgb,p_fecha_benchmark){
  if(p_compresion){
    f_resultado <- funcion_compresion_fecha(p_tabla,p_variables_fecha ,p_variable_pedido, p_variables_cantidades,p_fecha_benchmark)
    f_tabla <- f_resultado$tabla
    f_variables_fecha <- f_resultado$fechas
    f_variables_benchmark <- f_resultado$fechas_benchmark
  }
  f_resultado <- funcion_solo_variables_maximo(f_tabla,c(f_variables_fecha,f_variables_benchmark))
  f_tabla <- f_resultado$tabla
  f_variables_fecha <- f_resultado$variables[-length(f_resultado$variables)]
  f_variables_benchmark <- f_resultado$variables[length(f_resultado$variables)]
  f_tabla_resumen <- funcion_parametros_grafica_2(f_tabla, p_variables_cantidades, f_variables_fecha)
  f_variables <- eval(parse(text = paste0('c(names(f_tabla_resumen$',p_tipo_fgb,'))')))
  f_valores <- eval(parse(text = paste0('c((f_tabla_resumen$',p_tipo_fgb,') %>% unlist %>% as.numeric)')))
  f_g <- funcion_grafica_barras(f_variables, f_valores, p_texto_label, p_texto_x, p_texto_y)
  return(f_g)
}

# funcion_main_grafica_2(p_tabla, p_compresion = TRUE, p_texto_x, p_texto_y, p_variables_fecha, p_variable_pedido, p_variables_cantidades, p_texto_label, p_tipo_fgb,p_variables_benchmark)
  


# (secondary - plotting) ------------------------------------------------------------------------------------------------------------------------------------------

# p_variables <- f_tabla_resumen$sumas %>% names
# p_valores <- f_tabla_resumen$sumas %>% unlist %>% as.numeric
# p_texto_label <- 'entregas'
# p_texto_x <- 'proceso'
# p_texto_y <- 'cantidad'


funcion_grafica_barras <- function(p_variables, p_valores, p_texto_label, p_texto_x, p_texto_y){
  
  f_tabla1 <- data.frame(variable = factor(p_variables, levels = p_variables), valor = p_valores)
  
  g1 <- ggplot(f_tabla1) +
    geom_col(aes(x = variable, y = valor), alpha = .5, fill = 'darkblue', color = 'black') +
    geom_label(aes(x = variable, y = valor, label = paste0(round(valor,1),' ',p_texto_label))) +
    xlab(p_texto_x) +
    ylab(p_texto_y)
  
  return(g1)
}

# (secondary - wrangling) resumen para la gráfica de barras -----------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- f_tabla
# p_variables_suma <- parametros$domestico_cantidades
# p_variables_cuantos <- f_variables_fecha

funcion_parametros_grafica_2 <- function(p_tabla, p_variables_suma,p_variables_cuantos){
  n <- nrow(p_tabla)
  sumas <- p_tabla %>% select(p_variables_suma) %>%
    summarise_all(.,.funs = sum_x)
  
  fechas <- p_tabla %>% select(p_variables_cuantos) %>%
    summarise_all(.,.funs = nona_x)
  
  resultado <- list()
  resultado$n <- n
  resultado$sumas <- sumas
  resultado$cuantos <- fechas
  
  return(resultado)
}

# (secondary - wrangling) comnpresión de fechas ------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_variable_agrupacion <- p_variable_pedido

funcion_compresion_fecha <- function(p_tabla, p_variables_fecha, p_variable_agrupacion,p_variables_cantidades,p_fecha_benchmark){
  funcion1 <- paste0('f_tabla <- p_tabla %>% dplyr::group_by(',p_variable_agrupacion,') %>% dplyr::summarise(')
  funcion2 <- paste0(p_variables_fecha,'_min = min(', p_variables_fecha, ', na.rm = T)', collapse = ',')
  funcion3 <- ','
  funcion4 <- paste0(p_variables_fecha,'_max = max(', p_variables_fecha, ', na.rm = T)', collapse = ',')
  funcion4a <- ','
  funcion4b <- paste0(p_variables_cantidades,' = sum(', p_variables_cantidades, ', na.rm = T)', collapse = ',')
  funcion5 <- ','
  funcion6a <- paste0(p_fecha_benchmark,'_min = min(', p_fecha_benchmark, ', na.rm = T)', collapse = ',')
  funcion6b <- ','
  funcion6c <- paste0(p_fecha_benchmark,'_max = max(', p_fecha_benchmark, ', na.rm = T)', collapse = ',')
  funcion7 <- paste0(') %>% data.frame')
  eval(parse(text = paste0(funcion1,funcion2,funcion3,funcion4,funcion4a,funcion4b,funcion5, funcion6a, funcion6b, funcion6c, funcion7)))
  for(i in 1:length(f_tabla)){                           # al parecer un infinito en fechas se ve como NA pero para fines prácticos
    f_tabla[,i][is.infinite(f_tabla[,i])] <- NA          # sigue siendo una fecha con valor infinito
  }
  f_fechas1 <- names(f_tabla)[str_detect(names(f_tabla),'min')]
  f_fechas2 <- names(f_tabla)[str_detect(names(f_tabla),'max')]
  
  f_fechas <- NULL
  
  for(i in 1:(length(f_fechas1)-1)){
    f_fechas <- c(f_fechas,f_fechas1[i])
    f_fechas <- c(f_fechas,f_fechas2[i])
  }
  
  f_fechas_benchmark <- NULL
  
    f_fechas_benchmark <- c(f_fechas_benchmark,f_fechas1[length(f_fechas1)])
    f_fechas_benchmark <- c(f_fechas_benchmark,f_fechas2[length(f_fechas2)])
  
  resultado <- list()
  resultado$tabla <- f_tabla
  resultado$fechas <- f_fechas
  resultado$fechas_benchmark <- f_fechas_benchmark
  return(resultado)
}

funcion_compresion_fecha_extremo <- function(p_tabla, p_variables_fecha, p_variable_agrupacion,p_variables_cantidades,p_fecha_benchmark){
  funcion1 <- paste0('f_tabla <- p_tabla %>% dplyr::group_by(',p_variable_agrupacion,') %>% dplyr::summarise(')
  funcion2 <- paste0(p_variables_fecha,'_min = min(', p_variables_fecha, ', na.rm = F)', collapse = ',')
  funcion3 <- ','
  funcion4 <- paste0(p_variables_fecha,'_max = max(', p_variables_fecha, ', na.rm = F)', collapse = ',')
  funcion4a <- ','
  funcion4b <- paste0(p_variables_cantidades,' = sum(', p_variables_cantidades, ', na.rm = T)', collapse = ',')
  funcion5 <- ','
  funcion6a <- paste0(p_fecha_benchmark,'_min = min(', p_fecha_benchmark, ', na.rm = F)', collapse = ',')
  funcion6b <- ','
  funcion6c <- paste0(p_fecha_benchmark,'_max = max(', p_fecha_benchmark, ', na.rm = F)', collapse = ',')
  funcion7 <- paste0(') %>% data.frame')
  eval(parse(text = paste0(funcion1,funcion2,funcion3,funcion4,funcion4a,funcion4b,funcion5, funcion6a, funcion6b, funcion6c, funcion7)))
  for(i in 1:length(f_tabla)){                           # al parecer un infinito en fechas se ve como NA pero para fines prácticos
    f_tabla[,i][is.infinite(f_tabla[,i])] <- NA          # sigue siendo una fecha con valor infinito
  }
  f_fechas1 <- names(f_tabla)[str_detect(names(f_tabla),'min')]
  f_fechas2 <- names(f_tabla)[str_detect(names(f_tabla),'max')]
  
  f_fechas <- NULL
  
  for(i in 1:(length(f_fechas1)-1)){
    f_fechas <- c(f_fechas,f_fechas1[i])
    f_fechas <- c(f_fechas,f_fechas2[i])
  }
  
  f_fechas_benchmark <- NULL
  
  f_fechas_benchmark <- c(f_fechas_benchmark,f_fechas1[length(f_fechas1)])
  f_fechas_benchmark <- c(f_fechas_benchmark,f_fechas2[length(f_fechas2)])
  
  resultado <- list()
  resultado$tabla <- f_tabla
  resultado$fechas <- f_fechas
  resultado$fechas_benchmark <- f_fechas_benchmark
  return(resultado)
}

# (secondary - wrangling) ordena variables de fecha ------------------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- f_tabla
# p_fechas <- f_variables_fecha

funcion_ordena_fechas <- function(p_tabla,p_fechas){
  f_medias <- p_tabla %>% summarise_at(.vars = p_fechas,.funs = mean_x)
  f_medias <- f_medias[order(f_medias)]
  f_tabla_sin_fechas <- p_tabla %>% select(names(p_tabla)[-c(match(p_fechas,names(p_tabla)))])
  f_tabla_con_fechas <- p_tabla %>% select(names(f_medias))
  f_tabla <- cbind(f_tabla_sin_fechas,f_tabla_con_fechas)
  f_resultado <- list()
  f_resultado$tabla <- f_tabla
  f_resultado$fechas <- names(f_medias)
  return(f_resultado)
}

# (secondary - extra) media para usar en summarise_at -----------------------------------------------------------------------------------------------------------------------------------------------

mean_x <- function(a){
  b <- mean(a, na.rm = T)
  return(b)
}

sum_x <- function(a){
  b <- sum(a, na.rm = T)
  if(is.na(b))b <- 0
  return(b)
}

nona_x <- function(a){
  b <- sum(!is.na(a))
  return(b)
}

# (secondary - sólo máximos) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

funcion_solo_variables_maximo <- function(p_tabla, p_variables){
  f_tabla <- p_tabla %>%
    dplyr::select(-contains('_min')) %>%
    data.frame
  
  
  f_variables <- p_variables[str_detect(p_variables,'_min',negate = TRUE)]
  resultado <- list()
  resultado$tabla <- f_tabla
  resultado$variables <- f_variables
  return(resultado)
}



# (secondary - plotting) gráfica directa -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- f_tabla
# p_variable_wrap <- p_variable_pedido
# p_variables_fecha <- f_variables_fecha
# p_variables_benchmark <- f_variables_benchmark

funcion_grafica_pedidos_puntos <- function(p_tabla, p_variables_fecha,p_variables_benchmark){
  
  p_tabla$n <- 1:nrow(p_tabla)
  
  f_tamano_bolas <- 14/(log(nrow(p_tabla)))
  
  f_vector_fechas <- paste0(p_variables_fecha,collapse = ',')
  
  f_colores <- rainbow(length(p_variables_fecha))
  
  eval(parse(text = paste0(
    'p_tabla <- p_tabla %>%
    mutate(
    fecha_min = pmin(',f_vector_fechas,', na.rm = T),
    fecha_max = pmax(',f_vector_fechas,', na.rm = T)
  )'
  )))
  # eval(parse(text = paste0(               pmin está buggeado y no elimina NA's
  #   'p_tabla <- p_tabla %>%
  #   mutate(
  # fecha_min = pmin(',f_vector_fechas,', na.rm = T)
  # )'
  # )))
  

  f_tabla_leyenda <- data.frame(
    variables = factor(p_variables_fecha,levels = p_variables_fecha),
    colores = f_colores,
    x = max(p_tabla$fecha_max, na.rm = T),
    y = 1
  )

  
  funcion0 <- paste0(
    'g <- ggplot(p_tabla)'
  )
  funcion1 <- paste0(
    '+ geom_segment(aes(x = fecha_min, xend = fecha_max, y = n, yend = n),color = "darkgrey")'
  )
  funcion2a <- paste0(
    '+ geom_point(aes(x = ',p_variables_benchmark,', y = n),color = "red",size = ',f_tamano_bolas * 1.5,', alpha = 1,shape = 108)',collapse = ''
  )
  funcion3a <- paste0(
    '+ geom_point(aes(x = ',p_variables_fecha,', y = n),color = "black",size = ',f_tamano_bolas * 1.3,', alpha = 1,shape = 18)',collapse = ''
  )
  funcion3b <- paste0(
    '+ geom_point(aes(x = ',p_variables_fecha,', y = n),color = "',f_colores,'",size = ',f_tamano_bolas,', alpha = 1,shape = 18)',collapse = ''
  )
  funcion4 <- paste0(
    '+ geom_point(data = f_tabla_leyenda, aes(x = x, y = y, color = variables), size = .1)'
  )
  funcion5 <- paste0(
    '+ scale_color_manual(labels = c("',paste0(p_variables_fecha,collapse = '","'),'"),values = c("',paste0(f_colores,collapse = '","'),'"))'
  )
  funcion6 <- paste0(
    '+ guides(colour = guide_legend(override.aes = list(size=5, alpha = 1)))'
  )
  funcion7 <- paste0(
    '+ scale_x_date(date_labels="%y %b %d",date_breaks  ="1 week")'
  )
  funcion8 <- paste0(
    '+ theme(axis.text.x = element_text(angle = 90, hjust = 1))'
  )
  

eval(parse(text = paste0(funcion0, funcion1, funcion2a, funcion3a, funcion3b, funcion4,funcion5, funcion6, funcion7, funcion8)))  

return(g)
  
}

# input <- list()
# input$input_filtro_zona <- 'Doméstico'
# tablas$sub <- tablas$domestico

# p_region <- f_region
# p_tabla <- tablas$sub


funcion_revisar_fechas_coherentes <- function(p_tabla,p_region){
  

    
    funcion1 <- paste0(
      'f_resumen <- data.frame('
    )
    funcion2 <- paste0(
      p_region$fechas[2:length(p_region$fechas)], ' = as.numeric(p_tabla$',p_region$fechas[2:length(p_region$fechas)], ' - p_tabla$',p_region$fechas[1],')',collapse = ','
    )
    funcion3 <- ')'
    
    # funcion4 <- 'geom_text(',,''
    
    
    eval(parse(text = paste0(funcion1,funcion2,funcion3)))
    
    f_resumen_m <- melt(f_resumen)
    
    
    g <- ggplot(f_resumen_m, aes(value, color = variable, fill = variable)) + 
      geom_density(adjust = 2, alpha = .5) + 
      xlim(0,50)
    
    return(g)
    
    
    
  
  
}




################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################
################

