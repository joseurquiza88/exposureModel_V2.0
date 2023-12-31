
#######################################################################
# ------------        TOTAL EXPOSURE ESTIMATE    -------------  
# ------------        Traditional method    -------------  
# Estimation of exposure assuming that the person remains at the place of origin 24 hours a day
traditional_model <- function (origin_point,date,dir_grid){
  #----    Spatial transformation
  coordinates(origin_point) <- ~longitude+latitude
  proj4string(origin_point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #----    Put the Coordinate reference system
  origin_point <- st_as_sf(origin_point, crs = 4326)
  #----    Use the "temporary_grid_search" function to search and estimate the daily concentration.
  
  hour_00 <- paste(date,"T00:01:00 -03",sep="")
  hour_23 <- paste(date,"T23:59:00 -03",sep="")
  grid_search<-temporary_grid_search(start_hour = hour_00, end_hour = hour_23,dir_grids=dir_grid,time_format="%Y-%m-%dT%H:%M:%S")
  
  
  #----    Take the point where the person with the average concentration of PM lives
  intersection_point <- st_intersection(origin_point ,grid_search)
  
  # Calculate the daily expossure -  μg m-3 - 24hs
  
  daily_exposure <- (intersection_point$value * 24)
  return(print(daily_exposure))
}

##------ Example

origin <- data.frame(longitude = c(-68.804999),
                     latitude = c(-32.891357))
date <- "2019-08-01"
#directorio local donde estan los archivos .shp
dir_grid <- "D:/Josefina/paper_git/paper_exposure_model/grid_example"

example <- traditional_model (origin_point = origin,date,dir_grid)
#Si genera error,generar una carpeta llamada "temp" dentro del directorio "dir_grid"
