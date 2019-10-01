library(rgdal)
library(leaflet)

# de aquí obtuve el mapa de méxico

# tmp <- tempdir()
# url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
# file <- basename(url)
# download.file(url, file)
# unzip(file, exdir = tmp)
# mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")

# con este código lo guardé para poder utilizarlo

# saveRDS(mexico,'mapaMexico.rds')


mexico <- readRDS('mapaMexico.rds')

mexico$fill_rate <- runif(32,60,90)

plot(mexico)

pal <- colorQuantile("YlGn", NULL, n = 5)

state_popup <- paste0("<strong>Estado: </strong>", 
                      mexico$name, 
                      "<br><strong>Fill Rate </strong>", 
                      paste0(round(mexico$fill_rate,1),'%'))


  