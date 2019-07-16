library(rgdal)
library(leaflet)



tmp <- tempdir()

url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"

file <- basename(url)

download.file(url, file)

unzip(file, exdir = tmp)

mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")
