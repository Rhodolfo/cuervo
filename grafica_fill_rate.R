library(highcharter)


datos_fill_rate <- data.frame(
  fecha = as.Date(c('2018-01-01','2018-02-02','2018-03-03','2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01','2018-09-01','2018-10-01','2018-11-01','2018-12-01','2019-01-01','2019-02-01','2019-03-01','2019-04-01','2019-05-01','2019-06-01','2019-07-01')),
  fill_rate = runif(19,60,80)
)


funcion_grafica_fill_rate <- function(tabla){
  hchart(
    tabla,
    type = 'line',
    hcaes(x = fecha, y = fill_rate, color = fill_rate)
  ) %>%
    hc_title(
      text = "<span style=\"color:#3385ff\"> Fill Rate </span> de los pedidos"
    ) %>%
    hc_subtitle(
      text = 'total'
    ) %>%
    hc_yAxis(
      tickPositions = c(0,20,40,60,80,100),
      gridLineColor = '#99ccff',
      labels = list(format = '{value} %', useHTML = TRUE),
      title = list(text = "fill rate")
    ) %>%
    hc_add_theme(hc_theme_economist())
}


  
