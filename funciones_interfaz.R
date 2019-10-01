

# interface con los filtros de doméstico -----------------------------------------------------------------------------------------------------------------------------------------

ui_filtros_domestico <- function(){     
  lista_opciones <- list()
  for(i in 1:length(parametros$domestico_filtros)){
    eval(parse(text=paste0('lista_opciones[[',i,']] <- unique(tablas$domestico$',parametros$domestico_filtros[i],')')))
  }
  vector_opciones <- NULL
  for(i in 1:length(lista_opciones)){
    lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
    vector_opciones <- c(vector_opciones,lista_opciones[[i]])
  }
  funcion1 <- 'tagList('
  funcion2 <- paste0('div(style="display:inline-block",pickerInput("input_filtro_',parametros$domestico_filtros,'",
      "',parametros$domestico_filtros,'",selected = NULL,multiple = TRUE,choices = c(',vector_opciones,')))')
  funcion2 <- paste0(funcion2,collapse = ',')    
  funcion3 <- ')'
  resultado <- eval(parse(text = paste0(
    funcion1,
    funcion2,
    funcion3
  )))
  return(resultado)
}

# interface con los filtros de usa -----------------------------------------------------------------------------------------------------------------------------------------------------

ui_filtros_usa <- function(){     
  lista_opciones <- list()
  for(i in 1:length(parametros$usa_filtros)){
    eval(parse(text=paste0('lista_opciones[[',i,']] <- unique(tablas$usa$',parametros$usa_filtros[i],')')))
  }
  vector_opciones <- NULL
  for(i in 1:length(lista_opciones)){
    lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
    vector_opciones <- c(vector_opciones,lista_opciones[[i]])
  }
  funcion1 <- 'tagList('
  funcion2 <- paste0('div(style="display:inline-block",pickerInput("input_filtro_',parametros$usa_filtros,'",
                     "',parametros$usa_filtros,'",selected = NULL,multiple = TRUE,choices = c(',vector_opciones,')))')
  funcion2 <- paste0(funcion2,collapse = ',')    
  funcion3 <- ')'
  resultado <- eval(parse(text = paste0(
    funcion1,
    funcion2,
    funcion3
  )))
  return(resultado)
}

# interface con los filtros de resto del mundo -----------------------------------------------------------------------------------------------------------------------------------------

ui_filtros_row <- function(){    
  lista_opciones <- list()
  for(i in 1:length(parametros$row_filtros)){
    eval(parse(text=paste0('lista_opciones[[',i,']] <- unique(tablas$row$',parametros$row_filtros[i],')')))
  }
  vector_opciones <- NULL
  for(i in 1:length(lista_opciones)){
    lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
    vector_opciones <- c(vector_opciones,lista_opciones[[i]])
  }
  funcion1 <- 'tagList('
  funcion2 <- paste0('div(style="display:inline-block",pickerInput("input_filtro_',parametros$row_filtros,'",
      "',parametros$row_filtros,'",selected = NULL,multiple = TRUE,choices = c(',vector_opciones,')))')
  funcion2 <- paste0(funcion2,collapse = ',')    
  funcion3 <- ')'
  resultado <- eval(parse(text = paste0(
    funcion1,
    funcion2,
    funcion3
  )))
  return(resultado)
}