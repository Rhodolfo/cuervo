# (main) función para graficas sobre resumenes ------------------------------------------------------------------------------------------------

# p_fecha_focal <- 'fecha_creacion'
# p_tabla <- tablas$zsdr159
# p_compresion <- TRUE



funcion_grafica_tiempos_grande <- function(p_tabla, p_fecha_focal, p_compresion = FALSE,p_texto_x,p_texto_y,p_variables_sobrantes_inicio){
  if(p_compresion){
    p_tabla <- funcion_compresion_fecha(p_tabla, 'pedido_sap')
    p_fecha_focal <- paste0(p_fecha_focal,'_min')
  }
  f_tabla_centrada <- funcion_fechas_centradas(p_tabla,p_fecha_focal)
  f_lista_tablas <- funcion_lista_tablas_fechas(f_tabla_centrada)
  f_resumen <- lapply(f_lista_tablas,funcion_resumen_tiempos)
  f_tabla_resumen <- do.call('rbind.fill',lapply(f_resumen,funcion_tabla1_tabla2))
  f_tabla_resumen <- funcion_compresion_tabla(f_tabla_resumen)
  g <- funcion_grafica_tiempos_puntos(f_tabla_resumen,p_texto_x,p_texto_y,p_variables_sobrantes_inicio)
  return(g)
}



# (main) función gráficas pedidos abiertos ----------------------------------------------------------------------------------



# p_fecha_focal <- 'fecha_pedido_min'
# p_fecha_criterio <- 'fecha_entrega_real'
# p_compresion <- FALSE
# input <- list()
# input$input_fecha_final_pa_141 <- 'fecha_original_preferente'
# input$input_filtro_fecha_1 <- '2019-09-01'
# input$input_filtro_fecha_2 <- '2019-09-15'
# eval(parse(text = paste0(
#   "tablas$vis_pa <- tablas$zsdr141 %>%
#   dplyr::filter(!is.na(region_nombre)) %>%
#   dplyr::filter(region_nombre == 'USA') %>%
#   dplyr::filter(",input$input_fecha_final_pa_141,">= '",input$input_filtro_fecha_1,"') %>%
#   dplyr::filter(",input$input_fecha_final_pa_141,"<= '",input$input_filtro_fecha_2,"')"
# )))
# p_tabla <- tablas$vis_pa

# funcion_grafica_tiempos_desagregados <- function(p_tabla, p_fecha_focal, p_compresion = FALSE,p_texto_x,p_texto_y){
# 
#   p_tabla <- funcion_compresion_fecha(p_tabla, 'pedido_sap')
#   
#   f_tabla_centrada <- funcion_fechas_centradas_con_fecha(p_tabla,p_fecha_focal)
#   f_lista_tablas <- funcion_lista_tablas_fechas(f_tabla_centrada)
# 
#   
#   funcion_grafica_tiempos_fecha(f_lista_tablas[[1]],'a','b')
#   
#   g <- funcion_grafica_tiempos(f_tabla_resumen,p_texto_x,p_texto_y)
#   return(g)
# }

# función de construcción de compresión de fechas para la tabla zsdr159 -------------------------------------------------------

# p_tabla <- tablas$zsdr159
# p_variable_agrupacion <- 'pedido_sap'

funcion_compresion_fecha <- function(p_tabla, p_variable_agrupacion){
  funcion1 <- paste0(
    'f_tabla <- p_tabla %>%
    group_by(',
    p_variable_agrupacion,
    ') %>%
    summarise('
  )
  funcion2 <- paste0(
    names(p_tabla)[str_detect(names(p_tabla),'fecha')],'_min = min(', names(p_tabla)[str_detect(names(p_tabla),'fecha')], ', na.rm = T)', collapse = ','
  )
  funcion3 <- ','
  
  funcion4 <- paste0(
    names(p_tabla)[str_detect(names(p_tabla),'fecha')],'_max = max(', names(p_tabla)[str_detect(names(p_tabla),'fecha')], ', na.rm = T)', collapse = ','
  )
  funcion5 <- paste0(
    ') %>% data.frame'
  )
  eval(parse(text = paste0(
    funcion1,
    funcion2,
    funcion3,
    funcion4,
    funcion5
  )))
}

# función para centrar las fechas ---------------------------------------------------------------------------------------------
# esta cosa toma una tabla, y centra un conjunto de fechas tomando como día 0 la fecha focal, restando esta fecha focal de todas las demás



funcion_fechas_centradas <- function(p_tabla,p_fecha_focal){
  f_tabla <- funcion_extrae_fechas(p_tabla)
  cat(names(f_tabla))
  cat('\n')
  f_fechas <- names(f_tabla)
  cat(f_fechas)
  cat('\n')
  cat(p_fecha_focal)
  cat('\n')
  f_fechas <- f_fechas[-match(p_fecha_focal,f_fechas)]
  cat(f_fechas)
  cat('\n')
  f_funcion_1 <- 'f_tabla_transformada <- f_tabla %>% mutate('
  f_funcion_2 <- paste0(c(f_fechas,p_fecha_focal),' = as.numeric(', c(f_fechas,p_fecha_focal), ' - ', p_fecha_focal,')',collapse = ',')
  f_funcion_3 <- ')'
  f_funcion_completa <- paste0(f_funcion_1,f_funcion_2,f_funcion_3)
  eval(parse(text = f_funcion_completa))
  f_tabla_transformada <- cbind(p_tabla[,match(p_fecha_focal,names(p_tabla))],f_tabla_transformada)
  names(f_tabla_transformada)[1] <- 'fecha_inicio'
  f_tabla_transformada <- cbind(p_tabla$pedido_sap,f_tabla_transformada)
  names(f_tabla_transformada)[1] <- 'pedido'
  f_tabla_transformada <- data.frame(f_tabla_transformada)
  f_tabla_transformada <- do.call(data.frame,lapply(f_tabla_transformada, function(x) replace(x, is.infinite(x),NA)))
  return(f_tabla_transformada)
}

funcion_fechas_centradas_con_fecha <- function(p_tabla,p_fecha_focal){
  f_tabla <- funcion_extrae_fechas(p_tabla)
  cat(names(f_tabla))
  cat('\n')
  f_fechas <- names(f_tabla)
  cat(f_fechas)
  cat('\n')
  cat(p_fecha_focal)
  cat('\n')
  f_fechas <- f_fechas[-match(p_fecha_focal,f_fechas)]
  cat(f_fechas)
  cat('\n')
  f_funcion_1 <- 'f_tabla_transformada <- f_tabla %>% mutate('
  f_funcion_2 <- paste0(c(f_fechas,p_fecha_focal),' =', c(f_fechas,p_fecha_focal),' + as.numeric(', c(f_fechas,p_fecha_focal), ' - ', p_fecha_focal,')',collapse = ',')
  f_funcion_3 <- ')'
  f_funcion_completa <- paste0(f_funcion_1,f_funcion_2,f_funcion_3)
  eval(parse(text = f_funcion_completa))
  f_tabla_transformada <- cbind(p_tabla[,match(p_fecha_focal,names(p_tabla))],f_tabla_transformada)
  names(f_tabla_transformada)[1] <- 'fecha_inicio'
  f_tabla_transformada <- cbind(p_tabla$pedido_sap,f_tabla_transformada)
  names(f_tabla_transformada)[1] <- 'pedido'
  f_tabla_transformada <- data.frame(f_tabla_transformada)
  f_tabla_transformada <- do.call(data.frame,lapply(f_tabla_transformada, function(x) replace(x, is.infinite(x),NA)))
  return(f_tabla_transformada)
}


funcion_extrae_fechas <- function(p_tabla){ # función extrae variables de fecha 
  f_tabla <- p_tabla %>%
    dplyr::select(contains('fecha')) %>%
    dplyr::select(-contains('ex_fecha_'))
  return(f_tabla)
}


# función para separar tablas de fechas en múltiples tablas completas ------------------------------------------------------------

# p_tabla <- f_tabla_centrada

funcion_lista_tablas_fechas <- function(p_tabla){
  f_tabla_resto <- p_tabla
  f_lista_tablas <- list()
  i <- 1
  while(nrow(f_tabla_resto) > 0){
   f_cuantos <- 0
   f_test <- 0
    while(f_test == 0){
      f_variables_vacias <- f_tabla_resto %>%
        summarise_all(.funs = funcion_suma_na)
      f_tabla_resto <- funcion_quita_variables_vacias(f_tabla_resto,f_variables_vacias,f_cuantos)
      f_test <- f_tabla_resto %>%
        filter(complete.cases(.)) %>%
        nrow
      f_cuantos <- f_cuantos + 1
    }
    f_lista_tablas[[i]] <- f_tabla_resto %>%
      filter(complete.cases(.))
    f_tabla_resto <- f_tabla_resto %>%
      filter(!complete.cases(.))
    i <- i +1
  }
  return(f_lista_tablas)
}

funcion_suma_na <- function(a){ # función para obtener la suma de na's
  b <- sum(!is.na(a))
  b <- unlist(b)
  return(b)
}

funcion_quita_variables_vacias <- function(p_tabla,p_tablita,p_cuantos){ # función para quedarnos sólo con las variables que tienen datos completos
  f_tabla_completa <- p_tabla[,(p_tablita > p_cuantos)]
  return(f_tabla_completa)
}

# función para calcular el resumen de una tabla de tiempos ------------------------------------------------------------------

funcion_resumen_tiempos <- function(p_tabla){
  f_vector <- p_tabla %>%
    summarise_all(.funs = mean) %>%
    unlist
  f_tabla <- data.frame(variable = names(f_vector[-c(1,2)]),valor = f_vector[-c(1,2)]) %>%
    arrange(valor)
  f_tabla$fecha <- p_tabla$fecha_inicio[1]
  f_tabla$n <- nrow(p_tabla)
  f_tabla$pedido <- p_tabla$pedido[1]
  return(f_tabla)
}

# función para juntar varios resumenes en un sólo dataframe ---------------------------------------------------------------

funcion_tabla1_tabla2 <- function(p_tabla){
  funcion0 <- paste0(
    'f_tabla <- data.frame('
  )
  funcion1 <- paste0(
    'fecha = "',p_tabla$fecha[1],
    '",n = ',p_tabla$n[1],
    ',pedido = ',p_tabla$pedido[1],
    ','
  )
  funcion2 <- paste0(
    p_tabla$variable,' = ',p_tabla$valor, collapse = ','
  )
  funcion3 <- ')'
  funcion_completa <- paste0(funcion0,funcion1,funcion2,funcion3)
  eval(parse(text = funcion_completa))
  return(f_tabla)
}

# función de compresión de tabla -------------------------------------------------------------------------------------------

funcion_compresion_tabla <- function(p_tabla){
  f_tabla <- p_tabla[!duplicated(lapply(p_tabla, summary))]
  return(f_tabla)
}



# función para graficar un dataframe de tiempos ----------------------------------------------------------------------------

# p_tabla <- f_tabla_resumen
# p_texto_x <- 'x'
# p_texto_y <- 'y'
# p_variables_sobrantes_inicio <- 3





funcion_grafica_tiempos <- function(p_tabla, p_texto_x, p_texto_y,p_variables_sobrantes_inicio){
  
  f_items_previos <- 3
  colores <- rainbow(length(p_tabla) - (f_items_previos + 1))
  funcion0 <- paste0(
    ' ggplot(p_tabla) + '
  )
  funcion1_lista <- list()
  k <- 1
  for(j in 1:nrow(p_tabla)){
    f_tabla <- p_tabla[j,]                 # ordeno cronológicamente las variables
    f_tabla <- f_tabla[,!is.na(f_tabla)]
    f_orden <- order(unlist(f_tabla[-c(1:f_items_previos)])) + f_items_previos
    f_tabla <- f_tabla[,c(1:f_items_previos,f_orden)]
    f_colores <- colores[match(names(f_tabla)[(f_items_previos + 2):length(f_tabla)],names(p_tabla)[(f_items_previos + 1):length(p_tabla)]) - 1]
    for(i in (f_items_previos + 1):(length(f_tabla) - 1)){     # creo el código para la serie
      funcion1_lista[[k]] <- paste0(
        'geom_rect(data = f_tabla,aes(xmin = ', f_tabla[1,i],',',
        'xmax = ',f_tabla[1,(i+1)],',',
        'ymin = ',j-.3,',',
        'ymax = ',j+.3,'),fill = "',f_colores[i - f_items_previos],'", color = "black", alpha = 1)'
      )
      k <- k+1
    }
  }
  funcion2_tabla <- data.frame(
    y = (1:length(colores)) + nrow(p_tabla),
    x = 1,
    nombre = names(p_tabla[c((f_items_previos + 2):length(p_tabla))]),
    color = colores
  )
  funcion1 <- paste0(unlist(funcion1_lista),collapse = '+')
  funcion2 <- paste0(
    '+ geom_point(data = funcion2_tabla,aes(x=0,y=1,fill = color,colour = color)) + 
       geom_text(data = p_tabla, aes(x = rep(-1,nrow(p_tabla)), y = 1:nrow(p_tabla), label = paste0("n=",p_tabla$n))) +
    scale_color_manual(name = "proceso",labels= c(as.character(funcion2_tabla$nombre)), values = colores) + 
    guides(fill = FALSE,colour = guide_legend(override.aes = list(size=10))) + 
    xlab("',p_texto_x,'") + ylab("',p_texto_y,'")+ 
    theme(legend.position="bottom",
    legend.title = element_text(size=16),
    legend.text = element_text(size=16))'
  )
  funcion_completa <- paste0(
    funcion0,
    funcion1,
    funcion2
  )
  funcion_completa
  parse(text = funcion_completa)
  eval(parse(text = funcion_completa))
}


# (secondary) función para graficar un dataframe de tiempos ----------------------------------------------------------------------------

# p_tabla <- f_tabla_resumen

funcion_grafica_tiempos_fecha <- function(p_tabla, p_texto_x, p_texto_y){
  
  f_items_previos <- 3
  colores <- rainbow(length(p_tabla) - (f_items_previos + 1))
  funcion0 <- paste0(
    ' ggplot(p_tabla) + '
  )
  funcion1_lista <- list()
  k <- 1
  for(j in 1:nrow(p_tabla)){
    f_tabla <- p_tabla[j,]                 # ordeno cronológicamente las variables
    f_tabla <- f_tabla[,!is.na(f_tabla)]
    f_orden <- order(unlist(f_tabla[-c(1:f_items_previos)])) + f_items_previos
    f_tabla <- f_tabla[,c(1:f_items_previos,f_orden)]
    f_colores <- colores[match(names(f_tabla)[(f_items_previos + 2):length(f_tabla)],names(p_tabla)[(f_items_previos + 1):length(p_tabla)]) - 1]
    for(i in (f_items_previos + 1):(length(f_tabla) - 1)){     # creo el código para la serie
      funcion1_lista[[k]] <- paste0(
        'geom_rect(data = f_tabla,aes(xmin = as.Date("', f_tabla[1,i],'"),',
        'xmax = as.Date("',f_tabla[1,(i+1)],'"),',
        'ymin = ',j-.3,',',
        'ymax = ',j+.3,'),fill = "',f_colores[i - f_items_previos],'", color = "black", alpha = 1)'
      )
      k <- k+1
    }
  }
  funcion2_tabla <- data.frame(
    y = (1:length(colores)) + nrow(p_tabla),
    x = 1,
    nombre = names(p_tabla[c((f_items_previos + 2):length(p_tabla))]),
    color = colores
  )
  funcion1 <- paste0(unlist(funcion1_lista),collapse = '+')
  funcion2 <- paste0(
    '+ geom_point(data = funcion2_tabla,aes(x=as.Date(',max(p_tabla$fecha_original_preferente_max,na.rm=T) + 1,'),y=1,fill = color,colour = color)) + 
       geom_text(data = p_tabla, aes(x = rep(as.Date(',max(p_tabla$fecha_original_preferente_max,na.rm=T) + 2,')), y = 1:nrow(p_tabla), label = pedido)) +
    scale_color_manual(name = "proceso",labels= c(as.character(funcion2_tabla$nombre)), values = colores) + 
    guides(fill = FALSE,colour = guide_legend(override.aes = list(size=10))) + 
    xlab("',p_texto_x,'") + ylab("',p_texto_y,'")+ 
    theme(legend.position="bottom",
    legend.title = element_text(size=16),
    legend.text = element_text(size=16))+
  scale_x_date(labels = format("%Y-%m-%d"))'
  )
  funcion_completa <- paste0(
    funcion0,
    funcion1,
    funcion2
  )
  funcion_completa
  parse(text = funcion_completa)
  eval(parse(text = funcion_completa))
}

# p_tabla <- f_lista_tablas[[1]]

funcion_grafica_proceso_tiempos <- function(p_t){
  
  p_tabla$y <- 1:nrow(p_tabla)
  
  colores <- rainbow(length(p_tabla)-2)
  funcion1 <-  'ggplot(p_tabla) +'
  
  
  funcion2 <- paste0(
    'geom_rect(aes(
    xmin = ',names(p_tabla)[p_vari],',
    xmanx
    ),colour ="', colores,'", size = 3, alpha = .5)', collapse = '+'
  )
  
  funcion3 <- '+ ylab("bla")'
  
eval(parse(text = paste0(
  funcion1,
  funcion2
)))
    
  
}


# (secondary) gráfica de barras --------------------------------------------------------------------------------------------

# p_tabla <- f_tabla_resumen
# p_texto_x <- 'x'
# p_texto_y <- 'y'
# p_variables_sobrantes_inicio <- 3


funcion_grafica_tiempos_puntos <- function(p_tabla,p_texto_x,p_texto_y,p_variables_sobrantes_inicio){
  colores <- rainbow(length(p_tabla) - p_variables_sobrantes_inicio)
  
  p_tabla$y <- 1:nrow(p_tabla)
  
  f_valores_y <- rep(c(1,3,5),10)
  
  funcion1 <-  'ggplot(p_tabla) +'
  
  # funcion2 <- paste0(
  #   'geom_point(aes(
  #   x=',names(p_tabla)[-c(1:p_variables_sobrantes_inicio,length(p_tabla))],',
  #   y = ',f_valores_y[1:(length(p_tabla)-p_variables_sobrantes_inicio-1)],'
  #   ),colour ="black", alpha = .1, size = 15)', collapse = '+'
  # )
  
  funcion3 <- paste0(
    'geom_point(aes(
    x=',names(p_tabla)[-c(1:p_variables_sobrantes_inicio,length(p_tabla))],',
    y = ',f_valores_y[1:(length(p_tabla)-p_variables_sobrantes_inicio-1)],'
  ),colour ="', colores,'", alpha = .5, size = 14)', collapse = '+'
  )
  
  funcion4 <- paste0(
    'geom_text(aes(
    x=',names(p_tabla)[-c(1:p_variables_sobrantes_inicio,length(p_tabla))],',
    y = ',f_valores_y[1:(length(p_tabla)-p_variables_sobrantes_inicio-1)],',
    label = round(',names(p_tabla)[-c(1:p_variables_sobrantes_inicio,length(p_tabla))],',1)
  ),size = 6)', collapse = '+'
  )
  
  funcion5 <- '+ facet_wrap(~n,scales = "free", ncol = 1)'
  
  funcion6 <- '+ ylim(-3,9)'
  
  f_tabla_colores <- data.frame(colores = colores,proceso = names(p_tabla[-c(1:p_variables_sobrantes_inicio,length(p_tabla))]))
  
  funcion7 <- paste0('+ geom_point(data = f_tabla_colores,aes(x=(',max(p_tabla$fecha_original_preferente_min,na.rm=T) + 1,'),y=1,fill = colores,colour = colores))')
  
  funcion8 <- paste0('+scale_colour_manual(name = "proceso",labels= f_tabla_colores$proceso,  values = colores) + 
                     guides(fill = FALSE,colour = guide_legend(override.aes = list(size=10,alpha = .5,text)))  + 
                     theme(legend.text=element_text(size=12))' )
  
  eval(parse(text = paste0(
    funcion1,
    # funcion2,
    # '+',
    funcion3,
    '+',
    funcion4,
    funcion5,
    funcion6,
    funcion7,
    funcion8
  )))
  
}

# (terciary) función para acomodar una tabla (nas a la izquierda) --------------------------------------------------------------



#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

