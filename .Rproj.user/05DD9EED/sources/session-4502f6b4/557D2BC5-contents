
#######################################################################
# ------------             SEARCH THE TIME GRID     ------------- 

#This function allows you to enter 2 different dates
# one at the beginning and the other at the end we obtain the grid of interest
# In the event that the times of the dates are different, for example:
# start_time <- "2023-01-19 06:50:00 -03" -  end_time <- "2023-01-19 09:50:00 -03"
# The function searches the grids available for that period (06,07,08,09)
# and a pixel-by-pixel average is generated.

#The output is a data.frame ready to temporarily save to .shp

##  -- Grid search function
hourly_grid <- function(hour, time_format,dir){
  input_hour <- as.POSIXct(strptime(hour, format = time_format))
  hour_exposure<- hour(input_hour)
  exposure_day <- date(input_hour)
  #Change to the directory where the grids are located.
  setwd(dir)
  # Only the shape files
  file_list <- dir(dir,pattern = ".shp") ## ver otras opciones
  # The name of the shp file should be in the format %Y-%m-%d_%H%M
  table_files <-as.POSIXct(strptime( substr(file_list,1,15), format = "%Y-%m-%d_%H%M"))
  # Search file according to the day and hour
  searched_date <- which((date(table_files)) == exposure_day)
  table_files <- table_files[searched_date] 
  searched_hour <- which((hour(table_files))== hour_exposure)
  file <- table_files[searched_hour] 
  name_file<- paste(substr(file,1,10),"_",substr(file,12,13),substr(file,15,16),".shp",sep = "")
  return(name_file)
}

## -- Grid processing
temporary_grid_search <- function(start_hour, end_hour=NULL,dir,time_format, gridID, shapeValue){  
  # --- Function that looks for the grid (.shp) corresponding to the hour of interest entered
  
  trajectory_grid_rbind <- data.frame()
  # Init time
  only_start_hour <- hour(as.POSIXct(strptime(start_hour, format = time_format)))
  # End time
  only_end_hour <- hour(as.POSIXct(strptime(end_hour, format = time_format)))
  
  #  --- 
  if (is.null(end_hour)){

    df_start_grids <- st_read(hourly_grid(start_hour, time_format = time_format,dir),quiet = TRUE)
    df_start_grid <- st_transform(df_start_grids,crs = 4326)# Always transform the CRS in 4326
    #df_start_grid$value_sum <- df_start_grid$value
    
      }
  #  --- When there is only one grid
  else if (only_start_hour == only_end_hour ){

    trajectory_grid <- st_read(hourly_grid(start_hour, time_format = time_format,dir),quiet = TRUE)
    salida<-st_transform(trajectory_grid,crs = 4326)
    #salida$len <- 1
    #salida$value_sum <- salida$value
  }else{
    # --- When there are several grids we do an average per pixel
    for(j in only_start_hour:only_end_hour){

      if (j < 10){
        j_hour <- paste("0",j,sep = "")
      }else{
        j_hour <- j
      }
      
      day <- paste(substr(start_hour,1,10),paste(j_hour,":00:00",sep = ""), "-03",sep = " ")

      trajectory_grid <- st_read(hourly_grid(day, time_format = "%Y-%m-%d %H:%M:%S",dir),quiet = TRUE)
      trajectory_grid$hour <- day
      trajectory_grid_rbind <- rbind(trajectory_grid_rbind,trajectory_grid)
    }
    ## ------------ Group by the ID of the grid and make the mean of each pixel
    trajectory_grid_rbind %>%
      group_by(GRI1_ID) %>%  
      group_split() -> data_grilla
    
    df_grilla <- data.frame()
    for (p in 1:length(data_grilla)){

      # GRI1_ID <- data_grilla[[p]][["GRI1_ID"]][1]
      # dailyPM<- mean(data_grilla[[p]][["value"]],na.rm = T)
      GRI1_ID <- data_grilla[[p]][[gridID]][1]
      value <- mean(data_grilla[[p]][[shapeValue ]],na.rm = T)
      #value_sum <- sum(data_grilla[[p]][[pollutant]],na.rm = T)
      geometry <- data_grilla[[p]][["geometry"]][1]
      #len <- length(data_grilla[[p]][["geometry"]])
      
      df <- data.frame(gridID = GRI1_ID, value = value, 
                       geometry = geometry)#,len = len)#,value_sum

      df_grilla <- rbind(df_grilla ,df)
      #names(df_grilla) <- c(gridID,"value","geometry","len")#,"value_sum"
    }

    
    st_write(df_grilla,"./temp/temp_grid.shp",delete_layer = TRUE,quiet = TRUE)
    
    trajectory_grid<- st_read("./temp/temp_grid.shp",quiet = TRUE)
    salida<-st_transform(trajectory_grid,crs = 4326)
    
  }
  if(is.null(end_hour)){

    return(df_start_grid)
    }else{
      return(salida)
    }
}

