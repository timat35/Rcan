
.csu_tick_generator <- function(max,min = 0,log_scale=FALSE) {
  
  
  temp_log_max = 10^floor(log10(max))
  temp_unit_floor_max = floor(max/(temp_log_max))
  
  if (!log_scale) {
    
    if (temp_unit_floor_max < 2) {
      tick_space = 0.2*temp_log_max
    } else {
      if (temp_unit_floor_max < 5) {
        tick_space = 0.5*temp_log_max
      } else {
        tick_space = temp_log_max
      }
    }
    
    temp_top <- ceiling(max/tick_space)*tick_space
    tick_list <- seq(0, temp_top, tick_space)
    tick_minor_list <- NULL
    
  } else {
    
    temp_log_min <- 10^floor(log10(min))
    temp_unit_floor_min <- floor(min/(temp_log_min))
    
    if (temp_log_min == temp_log_max) {
      
      tick_list <- c(temp_unit_floor_min:(temp_unit_floor_max+1)*temp_log_min)
      
      
      if (temp_unit_floor_max == temp_unit_floor_min) {
        tick_minor_list <- c((temp_unit_floor_min*temp_log_min)+0:9*(temp_log_min/10)) 
      } else {
        tick_minor_list <- c((temp_unit_floor_min*temp_log_min)+0:19*(temp_log_min/10)) 
      }
      
    } else if (temp_log_max/temp_log_min < 1000) {
      
      
      if (temp_unit_floor_min < 6) {
        tick_list <- temp_unit_floor_min:5*temp_log_min 
        tick_list <- c(tick_list, temp_log_min*7) ## min . . 5 7
      } else  {
        tick_list <- temp_unit_floor_min*temp_log_min ## min 
      }
      
      tick_minor_list <- temp_unit_floor_min:19*temp_log_min ## min .  . 19
      
      while (temp_log_min != (temp_log_max/10)) {
        temp_log_min = temp_log_min*10 
        tick_list <- c(tick_list, c(1,2,3,5,7)*temp_log_min)
        tick_minor_list <- c(tick_minor_list, 2:19*temp_log_min)
      }
      
      tick_minor_list <- c(tick_minor_list, 2:(temp_unit_floor_max+1)*temp_log_max)
      
      if (temp_unit_floor_max <5) {
        tick_list <- c(tick_list, 1:(temp_unit_floor_max+1)*temp_log_max)
      } else if (temp_unit_floor_max <7) {
        tick_list <- c(tick_list, c(1,2,3,5,temp_unit_floor_max+1)*temp_log_max)
      } else {
        tick_list <- c(tick_list, c(1,2,3,5,7,temp_unit_floor_max+1)*temp_log_max)
      }
      
    } else {
      
      if (temp_unit_floor_min == 1) {
        tick_list <- c(1,2,3,5)*temp_log_min
      } else  if (temp_unit_floor_min == 2) {
        tick_list <- c(2,3,5)*temp_log_min
      } else  if (temp_unit_floor_min < 6) {
        tick_list <- c(5,7)*temp_log_min
      } else {
        tick_list <- 7*temp_log_min
      }
      tick_minor_list <- temp_unit_floor_min:9*temp_log_min ## min .  . 19
      
      while (temp_log_min != (temp_log_max/10)) {
        temp_log_min = temp_log_min*10 
        tick_list <- c(tick_list, c(1,2,5)*temp_log_min)
        tick_minor_list <- c(tick_minor_list, 2:9*temp_log_min)
      }
      
      tick_minor_list <- c(tick_minor_list, 2:(temp_unit_floor_max+1)*temp_log_max)
      
      if (temp_unit_floor_max <5) {
        tick_list <- c(tick_list, unique(c(1,2,temp_unit_floor_max+1)*temp_log_max))
      } else if (temp_unit_floor_max <6) {
        tick_list <-c(tick_list, c(1,2,5)*temp_log_max)
      } else if (temp_unit_floor_max < 7) {
        tick_list <- c(tick_list, c(1,2,5,7)*temp_log_max)
      } else {
        tick_list <- c(tick_list, c(1,2,5,7,temp_unit_floor_max+1)*temp_log_max)
      }
      
      
      
    }
    
  }
  
  return(list(tick_list=tick_list, tick_minor_list=tick_minor_list))
  
}

.csu_axes_label <- function(l) {
  
  l <- format(l, big.mark = ",", scientific = FALSE, drop0trailing = TRUE)
  
}

.csu_year_tick_generator <- function(min, max) {
  
  temp1 <- min - (min %% 5)
  temp2 <- max - (max %% 5) +5
  
  if (temp2- temp1 <= 30) {
    year_space <- 5 
    year_list <- seq(temp1,temp2,year_space)
    year_minor_list <- year_list
    
  } else  {
    year_space <- 10 
    if (temp1 %% 10 > 0) {
      year_list <- seq(temp1+5,temp2,year_space)
      year_minor_list <-  seq(temp1,temp2,year_space/2)
    } else {
      year_list <- seq(temp1,temp2,year_space)
      year_minor_list <-  seq(temp1,temp2,year_space/2)
    }
  }
  
  return(list(tick_list=year_list, tick_minor_list=year_minor_list))
  
}


.csu_format_export <- function(type, plot_title, landscape=FALSE) {
  
  #http://www.altelia.fr/actualites/calculateur-resolution-definition-format.htm
  
  # 6 inch = 15.24 cm
  #10,795

  
  png_width <- ifelse(landscape, 2339 , 1654 )
  png_height <- ifelse(landscape, 1654 , 2339 )
  tiff_width <- ifelse(landscape, 3508 , 2480 )
  tiff_height <- ifelse(landscape, 2480 , 3508 )
  pdf_width <- ifelse(landscape, 11.692 , 8.267 )    
  pdf_height <- ifelse(landscape, 8.267 , 11.692 )   
  
  if (type == "pdf") {
    
    pdf(paste(plot_title,".",type, sep=""),width = pdf_width, height = pdf_height) 
    
  } else if (type == "svg") {
    
    svg(paste(plot_title,".",type, sep=""),width = pdf_width, height = pdf_height) 
    
  } else if (type == "png") {
    
    png(paste(plot_title,".",type, sep=""),width = png_width, height = png_height, units = "px",res = 200) 
    
    
  } else if (type == "tiff") {
    
    tiff(paste(plot_title,".",type, sep=""),width = tiff_width, height = tiff_height,units = "px",res = 300,compression ="lzw")
  }
}