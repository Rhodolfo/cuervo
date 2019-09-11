# función para graficas procesos ------------------------------------------------------------------------------------------------

p_fecha_focal <- 'fecha_pedido'
p_tabla <- tablas$zsdr141 %>% filter(region_nombre == 'USA')

funcion_grafica_tiempos_grande <- function(p_tabla,p_focal){
  f_tabla_centrada <- funcion_fechas_centradas(p_tabla,p_focal)
  f_lista_tablas <- funcion_lista_tablas_fechas(f_tabla_centrada)
  f_resumen <- lapply(f_lista_tablas,funcion_resumen_tiempos)
  f_tabla_resumen <- do.call('rbind.fill',lapply(f_resumen,funcion_tabla1_tabla2))
  f_tabla_resumen <- funcion_compresion_tabla(f_tabla_resumen)
}



# función para centrar las fechas ---------------------------------------------------------------------------------------------
# esta cosa toma una tabla, y centra un conjunto de fechas tomando como día 0 la fecha focal, restando esta fecha focal de todas las demás



funcion_fechas_centradas <- function(p_tabla,p_fecha_focal){
  f_tabla <- funcion_extrae_fechas(p_tabla)
  f_fechas <- names(f_tabla)
  f_fechas <- f_fechas[-match(p_fecha_focal,f_fechas)]
  f_funcion_1 <- 'f_tabla_transformada <- f_tabla %>% mutate('
  f_funcion_2 <- paste0(c(f_fechas,p_fecha_focal),' = as.numeric(', c(f_fechas,p_fecha_focal), ' - ', p_fecha_focal,')',collapse = ',')
  f_funcion_3 <- ')'
  f_funcion_completa <- paste0(f_funcion_1,f_funcion_2,f_funcion_3)
  eval(parse(text = f_funcion_completa))
  f_tabla_transformada <- cbind(p_tabla[,match(p_fecha_focal,names(p_tabla))],f_tabla_transformada)
  names(f_tabla_transformada)[1] <- 'fecha_inicio'
  f_tabla_transformada <- cbind(p_tabla$pedido_sap,f_tabla_transformada)
  names(f_tabla_transformada)[1] <- 'pedido'
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

p_tabla <- f_tabla_resumen

funcion_grafica_tiempos <- function(p_tabla){
  
  colores <- rainbow(length(p_tabla) - 4)
  
  
  funcion0 <- paste0(
    ' ggplot(p_tabla) + '
  )
  
  
  funcion1_lista <- list()
  
  k <- 2
  for(j in 1:nrow(p_tabla)){
    
    
    f_tabla <- p_tabla[j,]                 # ordeno cronológicamente las variables
    f_tabla <- f_tabla[,!is.na(f_tabla)]
    f_orden <- order(unlist(f_tabla[-c(1:3)])) + 3
    f_tabla <- f_tabla[,c(1,2,3,f_orden)]
    f_colores <- colores[match(names(f_tabla)[4:length(f_tabla)],names(p_tabla)[4:length(p_tabla)])]
    
    
    for(i in 4:(length(f_tabla) - 1)){     # creo el código para la serie
      funcion1_lista[[k]] <- paste0(
        'geom_rect(data = f_tabla,aes(xmin = ', f_tabla[1,i],',',
        'xmax = ',f_tabla[1,(i+1)],',',
        'ymin = ',j-.3,',',
        'ymax = ',j+.3,'),fill = "',f_colores[i-3],'", color = "black", alpha = .5)'
      )
      k <- k+1
    }
    
    
    
  }
  
  funcion2_tabla <- data.frame(
    y = (1:length(colores)) + nrow(p_tabla),
    x = 1,
    nombre = names(p_tabla[c(4:length(p_tabla))]),
    color = colores
  )
  
  funcion2_tabla <- funcion2_tabla[1:(nrow(tabla) - 1),]
    
  
  
  funcion1 <- paste0(unlist(funcion1_lista),collapse = '+')
  

  
  funcion2 <- paste0(
    '+ geom_point(data = funcion2_tabla,aes(x=x,y=y,fill = color,colour =color))'
  )
  
  funcion_completa <- paste0(
    funcion0,
    funcion1,
    funcion2
  )
  
  funcion_completa
  parse(text = funcion_completa)
  
  eval(parse(text = funcion_completa))
  
  g +
  scale_fill_manual(name = 'bla', values = c('red' = 'red'), labels = c('blaa'))
  
}


geom_rect(data=mtcars[1,], aes(xmin=100, xmax=200, ymin=0,ymax=Inf), fill="red", alpha=0.2)


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


# función para graficar un proceso a partir de una tabla de resumen -----------------------------------------------------------

funcion_grafica_tiempos <- function(p_lista){
  f_funcion_1 <- 'ggplot(p_tabla) +'
  
  f_funcion_2_sub <- NULL
  for(i in 500:670){
    f_funcion_2_sub <- c(f_funcion_2_sub,paste0(
      'geom_segment(aes(x = f_tabla$', names(f_tabla)[1],'[',i,'],',
      'y = f_tabla$entrega[',i,'],',
      'xend = f_tabla$', names(f_tabla)[2],'[',i,'],',
      'yend = f_tabla$entrega[',i,']),colour="black")')
    )
  }
  f_funcion_2 <- paste0(paste0(f_funcion_2_sub,collapse = '+'),'+')
  
  f_funcion_2a_sub <- NULL
  for(i in 500:670){
    f_funcion_2a_sub <- c(f_funcion_2a_sub,paste0(
      'geom_segment(aes(x = f_tabla$', names(f_tabla)[2],'[',i,'],',
      'y = f_tabla$entrega[',i,'],',
      'xend = f_tabla$', names(f_tabla)[3],'[',i,'],',
      'yend = f_tabla$entrega[',i,']),colour="red")')
    )
  }
  f_funcion_2a <- paste0(paste0(f_funcion_2a_sub,collapse = '+'),'+')
  
  f_funcion_2b_sub <- NULL
  for(i in 500:670){
    f_funcion_2b_sub <- c(f_funcion_2b_sub,paste0(
      'geom_segment(aes(x = f_tabla$', names(f_tabla)[3],'[',i,'],',
      'y = f_tabla$entrega[',i,'],',
      'xend = f_tabla$', names(f_tabla)[4],'[',i,'],',
      'yend = f_tabla$entrega[',i,']),colour="green")')
    )
  }
  f_funcion_2b <- paste0(paste0(f_funcion_2b_sub,collapse = '+'),'+')
  
  f_funcion_3 <- 'ylim(500, 670)'
  
  
  f_funcion <- paste0(
    f_funcion_1,
    f_funcion_2,
    f_funcion_2a,
    f_funcion_2b,
    f_funcion_3
  )
  
  eval(parse(text = f_funcion))
}

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


funcion_grafica_tiempos <- function(p_tabla,p_orden){
  
  
  
  f_tabla <- p_tabla[,match(names(p_tabla),p_orden)]
  
  f_tabla$entrega <- 1:nrow(f_tabla)  
  
  
  f_funcion_1 <- 'ggplot(p_tabla) +'
  
  f_funcion_2_sub <- NULL
  for(i in 500:670){
    f_funcion_2_sub <- c(f_funcion_2_sub,paste0(
      'geom_segment(aes(x = f_tabla$', names(f_tabla)[1],'[',i,'],',
      'y = f_tabla$entrega[',i,'],',
      'xend = f_tabla$', names(f_tabla)[2],'[',i,'],',
      'yend = f_tabla$entrega[',i,']),colour="black")')
    )
  }
  f_funcion_2 <- paste0(paste0(f_funcion_2_sub,collapse = '+'),'+')
  
  f_funcion_2a_sub <- NULL
  for(i in 500:670){
    f_funcion_2a_sub <- c(f_funcion_2a_sub,paste0(
      'geom_segment(aes(x = f_tabla$', names(f_tabla)[2],'[',i,'],',
      'y = f_tabla$entrega[',i,'],',
      'xend = f_tabla$', names(f_tabla)[3],'[',i,'],',
      'yend = f_tabla$entrega[',i,']),colour="red")')
    )
  }
  f_funcion_2a <- paste0(paste0(f_funcion_2a_sub,collapse = '+'),'+')
  
  f_funcion_2b_sub <- NULL
  for(i in 500:670){
    f_funcion_2b_sub <- c(f_funcion_2b_sub,paste0(
      'geom_segment(aes(x = f_tabla$', names(f_tabla)[3],'[',i,'],',
      'y = f_tabla$entrega[',i,'],',
      'xend = f_tabla$', names(f_tabla)[4],'[',i,'],',
      'yend = f_tabla$entrega[',i,']),colour="green")')
    )
  }
  f_funcion_2b <- paste0(paste0(f_funcion_2b_sub,collapse = '+'),'+')
  
  f_funcion_3 <- 'ylim(500, 670)'
  
  
  f_funcion <- paste0(
    f_funcion_1,
    f_funcion_2,
    f_funcion_2a,
    f_funcion_2b,
    f_funcion_3
  )
  
  eval(parse(text = f_funcion))
  
  
  geom_segment(aes(x = f_tabla$[1], y = f_tabla$))
  
  
  
}

funcion_graficas_proceso_general <- function(p_tablas){
  
  f_nombres <- names(p_tablas)[str_detect(string = names(p_tablas),pattern = 'fecha')]
  f_nombres <- f_nombres[as.logical(1-str_detect(string = f_nombres,pattern = 'ex_fecha_'))]
  
  f_tablas <- p_tablas %>%
    select(f_nombres) %>%
    filter(
      !is.na(fecha_acuse)
    ) %>%
    filter(
      !is.na(fecha_factura)
    )
  
  
  
  f_tablas_centradas <- funcion_fechas_centradas(f_tablas,'fecha_creacion',names(f_tablas)[-1])
  
  f_resumen_tiempos <- f_tablas_centradas %>%
    summarise_all(.funs = mean,na.rm=T) %>%
    unlist
  
  f_resumen_tiempos <- f_resumen_tiempos[!is.nan(f_resumen_tiempos)]
  
  f_resumen_tiempos <- f_resumen_tiempos[order(f_resumen_tiempos)]
  
  f_resumen_tiempos_tabla <- data.frame(
    proceso = names(f_resumen_tiempos)[2:length(f_resumen_tiempos)],
  )
  
  
  }


ggplot() + 
  geom_rect(
    data = f_resumen_tiempos,
    aes(
      xmin=1,
      xmax=2,
      ymin=f_resumen_tiempos[1],
      ymax = f_resumen_tiempos[2]
    ),
    color='black'
  )


ggplot(mtcars) +
  geom_density(aes(x=disp, group=cyl, fill=cyl), alpha=0.6, adjust=0.75) + 
  geom_rect(data=mtcars[1,], aes(xmin=100, xmax=200, ymin=0,ymax=Inf), fill="red", alpha=0.2)


for(i in 2:length(f_resumen_tiempos)){
  cat(names(f_resumen_tiempos)[i])
  cat('\n')
  cat(f_resumen_tiempos[i]-f_resumen_tiempos[1])
  cat('\n')
}


