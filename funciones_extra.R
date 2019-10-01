
# función para crear la variable f_region------------------------------------------------------------------------------------------------------------------------------------------------------------------

funcion_asigna_region <- function(p_region){
  f_region <- NULL
  
  if(p_region == 'USA')f_region <- 'usa'
  if(p_region == 'Resto del mundo')f_region <- 'row'
  if(p_region == 'Doméstico')f_region <- 'domestico'
  
  return(f_region)
}

