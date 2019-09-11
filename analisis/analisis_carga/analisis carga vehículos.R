d_transporte <- read_excel('Libro1.xlsx') %>%
  data.frame


head(d_transporte)

d_transporte <- d_transporte %>%
  mutate(
    mes_fecha_carga = month(Fecha.Carga),
    ano_fecha_carga = year(Fecha.Carga)
  )

d_transporte %>%
  group_by(
    ano_fecha_carga,
    mes_fecha_carga
  ) %>%
  summarise(conteo = n())


d_paletaje <- read_excel('Paletaje normal (actualizar) (2017).xls') %>%
  data.frame

head(d_paletaje)

d_track <- read_excel('zsdr159_2019_ene_ago.XLSX') %>%
  
  

head(d_track)

d_resultado <- d_track %>%
  setNames(                             #nombres
    .,str_replace_all(names(.),' ','_')
  ) %>%
  select(Pedido,Transporte,Material,Ctd._Ped._Cj._Nat.,Ctd._Ped._Cj._9_Lit,Material) %>%
  mutate(
    Material = as.numeric(Material)
  )
  

d_resultado$peso <- d_paletaje$PESO[match(d_resultado$Material,d_paletaje$CODIGO)]

d_resultado <- d_resultado %>%
  mutate(
    peso_nat = Ctd._Ped._Cj._Nat. * peso,
    peso_9lt = Ctd._Ped._Cj._9_Lit * peso
  )


d_resultado %>%
  group_by(
    Pedido,Transporte
  ) %>%
  summarise(
    peso_nat = sum(peso_nat,na.rm=T),
    peso_9lt = sum(peso_9lt,na.rm=T)
  )

d_tracks <- list()

d_tracks[[1]] <- read_excel('zsdr159_10_2019_ene_ago.XLSX')
d_tracks[[2]] <- read_excel('zsdr159_20_2019_ene_ago.XLSX')
d_tracks[[3]] <- read_excel('zsdr159_40_2019_ene_ago.XLSX')
d_tracks[[4]] <- read_excel('zsdr159_60_2019_ene_ago.XLSX')

d_track_completo <- do.call('rbind',d_tracks)

d_track_completo <- as.data.frame(d_track_completo)

write.csv(d_track_completo,'track159_2019_ene_ago_todos los canales.csv')
