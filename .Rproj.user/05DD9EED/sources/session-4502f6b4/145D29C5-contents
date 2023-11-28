# ---- Function to generate grids
#The function generates 24 hourly grids based on the entered boundaries 
# (xmin, ymin, xmax, ymax) with a specified resolution. The pixel size
# should be in meters. Coordinates are in latitude-longitude. 
# The files will be saved in shapefile format in the selected directory 
# 'dir' and named according to the entered date and the specified hour 
# (00-23h). 
#The output values in each file correspond to random values 
# between 0-250 according to the EPA AQI.


make_grid <- function(ymin,ymax,xmin,xmax, pixelSize,dir, date, values=NULL){
  #Conversion of values entered in meters to degrees
  latitude <- (ymin + ymax )/2
  # The circumference of the Earth at the equator in meters
  circumference_earth_equator <- 40075000  # Aprox. 40,075 km
  # Conversion
  degree_longitud <- (360 / (circumference_earth_equator * cos(latitude * pi / 180))) * pixelSize
  
  # Grid Generation
  # Defining spatial boundaries.Creating a 'bbox' object
  bbox <- st_bbox(c(xmin = xmin, ymin= ymin, xmax= xmax, ymax = ymax),crs = st_crs(4326))
  
  # Defining the grid resolution (in degrees)
  res <- degree_longitud
  
  # Creating 24 polygon grids
  grid <- st_make_grid(bbox, cellsize = c(res, res), what = "polygons")
    
  ID <- c(1:length(grid))
  # grid <- cbind(c(1:length(grid)) , grid) 
  for (i in 0:23){
    print(i)
    random_values <- sample(0:250, length(grid), replace = TRUE)
    
    sfc_polygon_values<- st_sf(ID,random_values, geometry = grid)
    names(sfc_polygon_values) <- c("GRI1_ID","value","geometry")
    if(i==0){
      name <- paste(date,"_","0",i,"01.shp",sep="")
    }
    else if(i<10 & i!=0){
    name <- paste(date,"_","0",i,"00.shp",sep="")
    }
    else{
      name <- paste(date,"_",i,"00.shp",sep="")
    }
    # Shapefile export
    st_write(sfc_polygon_values, paste(dir,name,sep=""))
    }
    
  }


#######
arriba-izq - abajo-deerecha
# Example
#Ejemplo chat
xmin = -80
xmax = -70
ymin = -40
ymax = -30
# Cordoba 
xmax = -64.08641884588135
xmin = -64.27816073530852 
ymax =  -31.320897547807217
ymin = -31.48431967197022  
# Mendoza
xmin = -68.93274
xmax = -68.67807
ymin= -33.05988
ymax = -32.78414 

#Mexico
#-31.44312344671951, -64.18100542226502 ARG
xmin = -99.26994216858604
xmax = -98.94861920239204 
ymin= 19.23859719684508
ymax = 19.541088423908583

#Madrid
xmin = -3.7752605696761523
xmax = -3.5700588517546783
ymin= 40.347562816507136
ymax = 40.49667032936084

pixelSize <- 350
# res <- 2
# res <- 0.003748915
dir <- "D:/Josefina/paper_git/paper_exposure_model/grid/"
#name <- "archivo5.shp"
date <- "2019-08-01"
make_grid(ymin,ymax,xmin,xmax, pixelSize,dir, date)
