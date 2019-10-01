library(googlesheets)
library(dplyr)

e1 = TRUE
while(e1 == TRUE){
  e1 = FALSE
  tryCatch(my_sheets <- gs_ls() ,error = function(e) {e1 <<- TRUE})
}

e1 = TRUE
while(e1 == TRUE){
  e1 = FALSE
  tryCatch(nube_cuervo <- gs_title('cuervo') ,error = function(e) {e1 <<- TRUE})
}

e1 = TRUE
while(e1 == TRUE){
  e1 = FALSE
  tryCatch(nube_cuervo_usuarios <- nube_cuervo %>% gs_read(ws = 'usuarios') ,error = function(e) {e1 <<- TRUE})
}
