

# (main) función para grafica 1 -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- tablas$domestico %>% dplyr::filter(Fecha_Are >= '2019-08-01') %>% dplyr::filter(Fecha_Are <= '2019-08-07')
# p_texto_x <- 'x'
# p_texto_y <- 'y'
# p_variables_fecha <- parametros$domestico_fechas
# p_variable_pedido <- parametros$domestico_pedido[1]
# p_compresion <- TRUE


funcion_main_grafica_1 <- function(p_tabla, p_compresion = FALSE,p_texto_x,p_texto_y,p_variables_fecha,p_variable_pedido){
  if(p_compresion){
    f_resultado <- funcion_compresion_fecha(p_tabla,p_variables_fecha ,p_variable_pedido)
    f_tabla <- f_resultado[[1]]
    f_variables_fecha <- f_resultado[[2]]
  }
  f_resultado <- funcion_solo_variables_maximo(f_tabla,f_variables_fecha)
  f_tabla <- f_resultado$tabla
  f_variables_fecha <- f_resultado$variables
  
  
  f_resultado2 <- funcion_ordena_fechas(f_tabla,f_variables_fecha)
  f_tabla <- f_resultado2$tabla
  f_variables_fecha <- f_resultado2$fechas
  f_grafica <- funcion_grafica_pedidos_puntos(f_tabla, f_variables_fecha)
  return(f_grafica)
}

# (main) función para grafica 2 -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_tabla <- tablas$domestico %>% dplyr::filter(Fecha_Are >= '2019-08-01') %>% dplyr::filter(Fecha_Are <= '2019-08-07')
# p_texto_x <- 'x'
# p_texto_y <- 'y'
# p_variables_fecha <- parametros$domestico_fechas
# p_variable_pedido <- parametros$domestico_pedido[1]
# p_compresion <- TRUE
p_variables_cantidades <- parametros$domestico_cantidades

funcion_main_grafica_2 <- function(p_tabla, p_compresion = FALSE,p_texto_x,p_texto_y,p_variables_fecha,p_variable_pedido){
  if(p_compresion){
    f_resultado <- funcion_compresion_fecha(p_tabla,p_variables_fecha ,p_variable_pedido, p_variables_cantidades)
    f_tabla <- f_resultado[[1]]
    f_variables_fecha <- f_resultado[[2]]
  }
}

# (secondary - wrangling) comnpresión de fechas ------------------------------------------------------------------------------------------------------------------------------------------------------------

# p_variable_agrupacion <- p_variable_pedido

funcion_compresion_fecha <- function(p_tabla, p_variables_fecha, p_variable_agrupacion,p_variables_cantidades){
  funcion1 <- paste0('f_tabla <- p_tabla %>%group_by(',p_variable_agrupacion,') %>%summarise(')
  funcion2 <- paste0(p_variables_fecha,'_min = min(', p_variables_fecha, ', na.rm = T)', collapse = ',')
  funcion3 <- ','
  funcion4 <- paste0(p_variables_fecha,'_max = max(', p_variables_fecha, ', na.rm = T)', collapse = ',')
  funcion4a <- ','
  funcion4b <- paste0(p_variables_cantidades,'_sum = sum(', p_variables_cantidades, ', na.rm = T)', collapse = ',')
  funcion5 <- paste0(') %>% data.frame')
  eval(parse(text = paste0(funcion1,funcion2,funcion3,funcion4,funcion4a,funcion4b,funcion5)))
  for(i in 1:length(f_tabla)){                           # al parecer un infinito en fechas se ve como NA pero para fines prácticos
    f_tabla[,i][is.infinite(f_tabla[,i])] <- NA          # sigue siendo una fecha con valor infinito
  }
  f_fechas <- names(f_tabla)[str_detect(names(f_tabla),'min')]
  f_fechas <- c(f_fechas,names(f_tabla)[str_detect(names(f_tabla),'max')])
  resultado <- list()
  resultado$tabla <- f_tabla
  resultado$fechas <- f_fechas
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

funcion_grafica_pedidos_puntos <- function(p_tabla, p_variables_fecha){
  
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

  
  funcion1 <- paste0(
    'g <- ggplot(p_tabla)'
  )
  funcion2 <- paste0(
    '+ geom_segment(aes(x = fecha_min, xend = fecha_max, y = n, yend = n),color = "darkgrey")'
  )
  funcion3 <- paste0(
    '+ geom_point(aes(x = ',p_variables_fecha,', y = n),color = "',f_colores,'",size = ',f_tamano_bolas,', alpha = 1,shape = 18,size = ',f_tamano_bolas,')',collapse = ''
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
  

eval(parse(text = paste0(funcion1,funcion2, funcion3, funcion4,funcion5, funcion6, funcion7, funcion8)))  

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

