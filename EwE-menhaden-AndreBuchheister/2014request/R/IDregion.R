library(rgdal); library(data.table)

IDregion <- function (x, shape, region.col = 'EPU', long.name = 'LON', 
                      lat.name = 'LAT'){
  
  x.orig <- as.data.table(copy(x))
  setnames(x.orig, c(long.name, lat.name), c('LON', 'LAT'))
  
  #Convert to spatial points data frame
  coordinates(x.orig) <- ~LON+LAT
  x.orig@proj4string  <- CRS('+init=epsg:4326') #Lat/Lon code
  lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
  x.projected <- spTransform(x.orig, lcc)
  
  #Identify tows within region
  shape.projected <- spTransform(shape, lcc)
  names(shape.projected@data)[which(names(shape.projected@data) == region.col)] <- "region"
  shape.projected$region <- over(x.projected, shape.projected)[ ,'region']
  names(shape.projected@data)[which(names(shape.projected@data) == "region")] <- region.col
  
  #Output data (convert spatial data frame back to lat/lon)
  x.new <- spTransform(x.projected, CRS('+init=epsg:4326'))
  x.data <- as.data.table(as.data.frame(x.new))
  x.data[, c('LAT', 'LON') := NULL]
  
  return(x.data)
}

#Example how to use
#load(file.path(dir, 'File.RData'))
#Assume this loads an R data.frame or data.table called fileobject
#epu.map <- readOGR(gis.dir, 'EPU') 
#The EPU here is the name of the shapefile without an extention

#out.data <- IDregion(fileobject, epu.map, region.col = 'EPU', lon.name = 'lon',
#                     lat.name = 'lat')

