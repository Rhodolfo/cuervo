
funcion_graficas_coherencia_ordinal <- function(fechas,titulo){
  # summary(fechas)
  
  l <- NULL
  for(i in 1:nrow(fechas)){
    u <- fechas[i,]
    v <- fechas[i,] %>% unlist %>% unique 
    w <- fechas[i,] %>% unlist %>% unique %>% order
    v <- v[w]
    l <- c(l,match(u,v))
  }
  m <- matrix(l,ncol = length(fechas),byrow = T)
  
  # head(fechas)
  # head(m)
  
  m <- data.frame(m)
  names(m) <- names(fechas)
  
  n<-m
  
  for(i in 1:nrow(m)){
    a <- m[i,]
    a_max <- max(a,na.rm=T)
    if(a_max > 1){
      aumento_total <- 0
      for(j in 2:a_max){
        aumento <- sum(!is.na(match(a,j+aumento_total-1)))-1
        if(aumento>0){
          a[which(a>=j+aumento_total)] <- a[which(a>=j+aumento_total)]+aumento
        }
        aumento_total <- aumento_total + aumento
      }
    }
    n[i,] <- a
  }
  
  
  o <- n %>%
    summarise_all(mean,na.rm=T) %>% unlist
  
  
  
  
  p <- melt(n)
  
  p$media <- o[match(p$variable,names(o))]
  p$constante <- 13.5
  
  q <- p %>% 
    select(variable,media,constante) %>%
    distinct
  
  
  g <- ggplot(p,aes(reorder(variable,-media),value)) +
    geom_boxplot() +
    geom_jitter(width = 0.2,height = .2,alpha = .1,color = 'blue',size=1) +
    geom_text(data = q, aes(reorder(variable,-media),constante,label = paste0('media: ',round(media,3)))) +
    scale_y_continuous(breaks = 1:15,limits = c(1,15)) +
    ggtitle(paste0('Coherencias ', titulo)) +
    xlab('proceso') + 
    ylab('orden de ejecución') +
    coord_flip()
  
  return(g)
}


funcion_graficas_coherencia_continua <- function(fechas,titulo){
  # summary(fechas)
  
  fechas_temp <- fechas %>%
    mutate(
      Fecha_Carga = as.numeric(Fecha_Carga - Fecha_Pedido),
      Fecha_Disponibilidad = as.numeric(Fecha_Disponibilidad - Fecha_Pedido),
      Fecha_Captura_SAP = as.numeric(Fecha_Captura_SAP - Fecha_Pedido),
      Fec._Entrega_a_Cte. = as.numeric(Fec._Entrega_a_Cte. - Fecha_Pedido),
      Fecha_Entrega = as.numeric(Fecha_Entrega - Fecha_Pedido),
      Fecha_Planeación_Transporte = as.numeric(Fecha_Planeación_Transporte - Fecha_Pedido),
      Fecha_Fact. = as.numeric(Fecha_Fact. - Fecha_Pedido),
      Fecha_cruce = as.numeric(Fecha_cruce - Fecha_Pedido),
      Fecha_de_Programación = as.numeric(Fecha_de_Programación - Fecha_Pedido),
      Fec._Entr._PT = as.numeric(Fec._Entr._PT - Fecha_Pedido),
      Fe_Imp.Fact. = as.numeric(Fe_Imp.Fact. - Fecha_Pedido),
      Fecha_Rec.BL = as.numeric(Fecha_Rec.BL- Fecha_Pedido),
      Fe.Guia_Fedex = as.numeric(Fe.Guia_Fedex - Fecha_Pedido),
      Fe.Orig.Pref = as.numeric(Fe.Orig.Pref - Fecha_Pedido),
      Lib.Calidad = as.numeric(Lib.Calidad - Fecha_Pedido),
      Fecha_Pedido = 0
    )
  
  n<-fechas_temp
  
  
  o <- n %>%
    summarise_all(mean,na.rm=T) %>% unlist
  
  
  
  
  p <- melt(n)
  
  p$media <- o[match(p$variable,names(o))]
  p$constante <- 13.5
  
  q <- p %>% 
    select(variable,media,constante) %>%
    distinct
  
  
  g <- ggplot(p,aes(reorder(variable,-media),value)) +
    geom_boxplot() +
    geom_jitter(width = 0.1,height = .1,alpha = .1,color = 'dodgerblue',size=.5) +
    geom_text(data = q, aes(reorder(variable,-media),max(p$value,na.rm=T),label = paste0('media: ',round(media,3)))) +
    ggtitle(paste0('Coherencias ', titulo)) +
    xlab('proceso') + 
    ylab('orden de ejecución') +
    coord_flip()
  
  return(g)
}

funcion_graficas_coherencia_continua_2 <- function(fechas,titulo){
  # summary(fechas)
  
  fechas <- fechas %>%
    filter(!is.na(Fec._Entr._PT))
  
  fechas_temp <- fechas %>%
    mutate(
      Fecha_Carga = as.numeric(Fecha_Carga - Fecha_de_Programación),
      Fecha_Disponibilidad = as.numeric(Fecha_Disponibilidad - Fecha_de_Programación),
      Fecha_Captura_SAP = as.numeric(Fecha_Captura_SAP - Fecha_de_Programación),
      Fec._Entrega_a_Cte. = as.numeric(Fec._Entrega_a_Cte. - Fecha_de_Programación),
      Fecha_Entrega = as.numeric(Fecha_Entrega - Fecha_de_Programación),
      Fecha_Planeación_Transporte = as.numeric(Fecha_Planeación_Transporte - Fecha_de_Programación),
      Fecha_Fact. = as.numeric(Fecha_Fact. - Fecha_de_Programación),
      Fecha_cruce = as.numeric(Fecha_cruce - Fecha_de_Programación),
      Fec._Entr._PT = as.numeric(Fecha_de_Programación - Fecha_de_Programación),
      Fe_Imp.Fact. = as.numeric(Fe_Imp.Fact. - Fecha_de_Programación),
      Fecha_Rec.BL = as.numeric(Fecha_Rec.BL- Fecha_de_Programación),
      Fe.Guia_Fedex = as.numeric(Fe.Guia_Fedex - Fecha_de_Programación),
      Fe.Orig.Pref = as.numeric(Fe.Orig.Pref - Fecha_de_Programación),
      Lib.Calidad = as.numeric(Lib.Calidad - Fecha_de_Programación),
      Fecha_Pedido = as.numeric(Fecha_Pedido - Fecha_de_Programación),
      Fecha_de_Programación = 0
    )
  
  n<-fechas_temp
  
  
  o <- n %>%
    summarise_all(mean,na.rm=T) %>% unlist
  
  
  
  
  p <- melt(n)
  
  p$media <- o[match(p$variable,names(o))]
  p$constante <- 13.5
  
  q <- p %>% 
    select(variable,media,constante) %>%
    distinct
  
  
  g <- ggplot(p,aes(reorder(variable,-media),value)) +
    geom_boxplot() +
    geom_jitter(width = 0.1,height = .1,alpha = .1,color = 'dodgerblue',size=.5) +
    geom_text(data = q, aes(reorder(variable,-media),max(p$value,na.rm=T),label = paste0('media: ',round(media,3)))) +
    ggtitle(paste0('Coherencias ', titulo)) +
    xlab('proceso') + 
    ylab('orden de ejecución') +
    coord_flip()
  
  return(g)
}


funcion_graficas_coherencia_continua_fecha_preferente<- function(fechas,titulo,variable){
  # summary(fechas)
  
  fechas <- fechas 
  
  fechas_temp <- fechas %>%
    mutate(
      ETD = as.numeric(ETD - Fe.Orig.Pref),
      ETA = as.numeric(ETA - Fe.Orig.Pref),
      Fecha_Carga = as.numeric(Fecha_Carga - Fe.Orig.Pref),
      Fecha_Disponibilidad = as.numeric(Fecha_Disponibilidad - Fe.Orig.Pref),
      Fec._Entrega_a_Cte. = as.numeric(Fec._Entrega_a_Cte. - Fe.Orig.Pref),
      Fecha_Entrega = as.numeric(Fecha_Entrega - Fe.Orig.Pref),
      Fecha_Planeación_Transporte = as.numeric(Fecha_Planeación_Transporte - Fe.Orig.Pref),
      Fecha_Fact. = as.numeric(Fecha_Fact. - Fe.Orig.Pref),
      Fecha_cruce = as.numeric(Fecha_cruce - Fe.Orig.Pref),
      Fec._Entr._PT = as.numeric(Fec._Entr._PT - Fe.Orig.Pref),
      Fe_Imp.Fact. = as.numeric(Fe_Imp.Fact. - Fe.Orig.Pref),
      Fecha_Rec.BL = as.numeric(Fecha_Rec.BL- Fe.Orig.Pref),
      Fe.Guia_Fedex = as.numeric(Fe.Guia_Fedex - Fe.Orig.Pref),
      Fecha_de_Programación = as.numeric(Fecha_de_Programación - Fe.Orig.Pref),
      Lib.Calidad = as.numeric(Lib.Calidad - Fe.Orig.Pref),
      Fecha_Pedido = as.numeric(Fecha_Pedido - Fe.Orig.Pref),
      Fecha_Captura_SAP = as.numeric(Fecha_Captura_SAP - Fe.Orig.Pref),
      Fe.Orig.Pref = 0
    )
  
  n<-fechas_temp
  
  
  o <- n %>%
    summarise_all(mean,na.rm=T) %>% unlist
  
  
  
  
  p <- melt(n)
  
  p$media <- o[match(p$variable,names(o))]
  p$constante <- 13.5
  
  q <- p %>% 
    select(variable,media,constante) %>%
    distinct
  
  
  g <- ggplot(p,aes(reorder(variable,-media),value)) +
    geom_boxplot() +
    geom_jitter(width = 0.1,height = .1,alpha = .1,color = 'dodgerblue',size=.5) +
    geom_text(data = q, aes(reorder(variable,-media),max(p$value+20,na.rm=T),label = paste0('media: ',round(media,3)))) +
    ggtitle(paste0('Coherencias ', titulo)) +
    xlab('proceso') + 
    ylab('orden de ejecución') +
    coord_flip()
  
  return(g)
}


