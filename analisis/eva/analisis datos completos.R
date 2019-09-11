library(lubridate)

datos <- funcion_carga_datos('zsdr141','zsdr141')


datos <- datos %>%
  mutate(
    mes_preferente = month(fecha_original_preferente),
    ano_preferente = year(fecha_original_preferente)
  ) %>%
  filter(
    !is.na(ano_preferente),
  ) %>%
  filter(ano_preferente == 2019) %>%
  filter(mes_preferente <= 8)

summary(datos)

completos <- datos %>%
  select(contains('ex_f'),contains('zona_ventas_nombre')) %>%
  group_by(zona_ventas_nombre) %>%
  summarise_if(is.logical,sum) %>%
  data.frame



for(i in 1:5){
  for(j in 19:2){
    completos[i,j] <- round(completos[i,j]/completos[i,2]*100,2)
  }
}


completos


write.csv(completos,'porcentaje de fechas completas en internacional.csv')


datos_incompletos <- datos %>%
  filter(
    is.na(fecha_factura)
  ) %>%
  filter(fecha_original_preferente < '2019-09-01')
  select(contains('fecha_')) %>%
  summary


write.csv(datos_incompletos,'pedidos incompletos 2019 hasta agosto.csv')



