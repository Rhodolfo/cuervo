library(networkD3)



funcion_parametros_sankey <- function(){
  pesos <- c(
    proceso$nacionales_moderno,
    proceso$nacionales_mayoreo,
    proceso$nacionales_consumo,
    proceso$nacionales_moderno + proceso$nacionales_mayoreo + proceso$nacionales_consumo,
    proceso$usa_wine,
    proceso$usa_proximo,
    proceso$usa_wine + proceso$usa_proximo,
    preceso$region1,
    proceso$region2,
    proceso$region3,
    proceso$region4,
    proceso$region5,
    proceso$region1 + proceso$region2 + proceso$region3 + proceso$region4 + proceso$region5
  )
  
  
  return(r)
}

#moderno



estructura_pedidos_nacional <- data.frame(
  source = c('moderno', 'mayoreo', 'centros_de_consumo'),
  target = c('nacional', 'nacional', 'nacional'),
  value = c(1,1,1)
)

estructura_pedidos_usa <- data.frame(
  source = c('wine_and_spirits', 'próximo'),
  target = c('usa', 'usa'),
  value = c(1,1)
)

estructura_pedidos_row <- data.frame(
  source = c('región_1', 'región_2', 'región_3', 'región_4', 'región_5'),
  target = c('row', 'row', 'row', 'row', 'row'),
  value = c(1,1,1,1,1)
)


links <- rbind(
  estructura_pedidos_nacional,
  estructura_pedidos_usa,
  estructura_pedidos_row
)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

links$group = as.factor(c('inactivo','en_proceso', 'terminado','en_proceso','en_proceso','terminado','terminado','terminado','terminado','terminado'))


# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["inactivo","en_proceso","terminado","moderno", "mayoreo","centros_de_consumo", "nacional", "wine_and_spirits", "próximo", "usa", "región_1", "región_2", "región_3", "región_4", "región_5", "row"]) .range(["gray","red", "green","green", "green" , "green", "green", "blue", "blue", "blue", "red", "red", "red", "red", "red", "red"])'

