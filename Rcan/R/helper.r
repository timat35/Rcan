
core.error_variable <- function(df_data, varname, funcname,type="numeric") {
  
  
  if (!(varname%in% colnames(df_data))) {
    
    stop(paste0(varname, " is not a column of ",   deparse(substitute(data_test)), " ,see documentation: Help(", deparse(substitute(funcname)), ")"))
    
  }
  
  if (type == "numeric") {
    if (!is.numeric(df_data[[varname]])) {
      
      
      stop(paste0(varname, " type must be ",type," see documentation: Help(", deparse(substitute(funcname)), ")"))
      
    }
    
  } else if (type == "character"){
    if (!is.character(df_data[[varname]])) {
      stop(paste0(varname, " type must be ",type," see documentation: Help(", deparse(substitute(funcname)), ")"))
      
    }
    
  } else if (type == "factor"){
    if (!is.factor(df_data[[varname]])) {
      stop(paste0(varname, " type must be ",type," see documentation: Help(", deparse(substitute(funcname)), ")"))
      
    }
  }
}





core.csu_tick_generator <- function(max,min = 0,log_scale=FALSE) {
  
  
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

core.csu_axes_label <- function(l) {
  
  l <- format(l, big.mark = ",", scientific = FALSE, drop0trailing = TRUE)
  
}

core.csu_year_tick_generator <- function(min, max) {
  
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


core.csu_format_export <- function(type, plot_title, landscape=FALSE) {
  
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


core.csu_asr <- function(df_data, var_age, var_cases, var_py, group_by=NULL,
                         var_age_group=NULL, missing_age = NULL,db_rate = 100000,
                         first_age = 1, last_age = 18, pop_base = "SEGI",
                         var_st_err=NULL,correction_info=FALSE, var_asr="asr", age_dropped = FALSE,
                         pop_base_count = NULL, age_label_list = NULL, Rcan_print=FALSE) {
  
  
  
  bool_dum_by <- FALSE
  bool_dum_age <- FALSE
  
  if (is.null(group_by)) {
    
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
    
  }
  
  
  
  if (is.null(var_age_group)) {
    
    df_data$CSU_dum_age <- "dummy_age_gr"
    var_age_group <- "CSU_dum_age"
    group_by <- c(group_by, "CSU_dum_age")
    bool_dum_age <- TRUE
    
  }
  
  
  dt_data <- data.table(df_data, key = group_by) 
  setnames(dt_data, var_age, "CSU_A")
  setnames(dt_data, var_cases, "CSU_C")
  setnames(dt_data, var_py, "CSU_P")

  # create index to keep order
  index_order <- c(1:nrow(dt_data))
  dt_data$index_order <- index_order
  
  # missing age 
  dt_data[dt_data$CSU_A==missing_age,CSU_A:=NA ] 
  dt_data[is.na(dt_data$CSU_A),CSU_P:=0 ] 
  
  #create age dummy: 1 2 3 4 --- 19
  dt_data$age_factor <- c(as.factor(dt_data$CSU_A))
  
  # correction factor 
  dt_data$correction <- 1 
  if (!is.null(missing_age)) {
    
    
    dt_data[, total:=sum(CSU_C), by=group_by] #add total
    dt_data[!is.na(dt_data$age_factor) , total_known:=sum(CSU_C), by=group_by] #add total_know
    dt_data$correction <- dt_data$total / dt_data$total_know 
    dt_data[is.na(dt_data$correction),correction:=1 ] 
    dt_data$total <- NULL
    dt_data$total_known <- NULL
    
  }
  
  if (is.null(pop_base_count)) {
    
    # create world population DF for different nb of age group
    SEGI_pop <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500)
    EURO_pop <- c(8000,7000,7000,7000,7000,7000,7000,7000,7000,7000,7000,6000,5000,4000,3000,2000,1000,1000)
    
    if (pop_base == "EURO") {
      pop <- EURO_pop
    } else {
      pop <- SEGI_pop
    }
    
    # calculated total pop for age selected 
    total_pop <- sum(pop[first_age:last_age])
    
    Standard_pop <- data.table(pop = pop, age_factor= c(1:18))
    
    pop[17] <- pop[17]+ pop[18]
    pop[18] <- 0
    Standard_pop$pop17 <- pop
    pop[16] <- pop[16]+ pop[17]
    pop[17] <- 0
    Standard_pop$pop16 <- pop
    pop[15] <- pop[15]+ pop[16]
    pop[16] <- 0
    Standard_pop$pop15 <- pop
    

    
    #age dropped option
    if (age_dropped) {
      dt_data$age_factor <- dt_data$age_factor + first_age -1   
    }
    
    
    # keep age selected 
    dt_data=dt_data[dt_data$age_factor %in% c(first_age:last_age) | is.na(dt_data$age_factor), ]
    
    # calculated maximum age group with population data
    if (last_age == 18) {
      dt_data <- merge(dt_data, dt_data[dt_data$CSU_P != 0,list(nb_age_group = max(age_factor)), by=var_age_group], by=var_age_group)  
    } else {
      dt_data$nb_age_group <- 18
    }
    
    # show population with less than 18 age group
    if (last_age == 18) {
      temp <- subset(dt_data,nb_age_group <18, select= c(var_age_group, "nb_age_group"))
      if (nrow(temp) >0) {
        setkey(temp,NULL)
        
        
        if (Rcan_print) {
        
          cat("\n")
          cat("Population with less than 18 age group:\n\n" )
          print
          print(unique(temp), row.names = FALSE)
          cat("\n")
          
        }
      }
      temp <- NULL
    }
    
    #regroup case for population with nb of age group <  18 
    for (i in 15:17) {
      
      if (i %in% dt_data$nb_age_group) {
        
        dt_data[nb_age_group == i & age_factor >= i , CSU_C:=sum(CSU_C), by=group_by] #add total_know
        dt_data[nb_age_group == i & age_factor > i & !is.na(age_factor), CSU_C := 0] 
        
      } 
    }
    
    #add world pop to database 
    dt_data <- merge(dt_data,Standard_pop, by =c("age_factor"), all.x=TRUE )

    Standard_pop <- NULL
    dt_data[nb_age_group==17, pop:=pop17]
    dt_data[nb_age_group==16, pop:=pop16]
    dt_data[nb_age_group==15, pop:=pop15]
    
    #return(dt_data)
    
  } else {
    
    #keep age group selected 
    dt_data <- dt_data[age_factor %in% (first_age:last_age), ]
    
    #calcul total pop for canreg
    total_pop <-sum(unique(dt_data[, c("age_factor", pop_base_count), with=FALSE])[[pop_base_count]])
    
    #get age group list variable
    if (is.null(age_label_list)) {
      age_label_list <- var_age
    }
    age_group_list <- as.character(unique(dt_data[[age_label_list]]))
    age_group_list <- paste(age_group_list,  collapse=" ")
    
    #rename variable population reference
    setnames(dt_data, pop_base_count, "pop")
  }
  
  #calcul ASR
  
  dt_data[dt_data$CSU_P != 0,rate:= dt_data$CSU_C[dt_data$CSU_P != 0]/ dt_data$CSU_P[dt_data$CSU_P != 0] * db_rate]
  dt_data$asr <- dt_data$rate * dt_data$pop
  dt_data[is.na(dt_data$asr),asr:=0 ] 
  
  dt_data$st_err <- ( dt_data$rate * (dt_data$pop^2) * (db_rate - dt_data$rate))/dt_data$CSU_P
  dt_data[is.na(dt_data$st_err),st_err:=0 ] 
  
  # to check order 
  dt_data<- dt_data[order(dt_data$index_order ),]
  dt_data<-  dt_data[,list( CSU_C=sum(CSU_C), CSU_P=sum(CSU_P),asr=sum(asr),st_err = sum(st_err),correction = max(correction)), by=group_by]
  
  dt_data$asr <- dt_data$asr / total_pop
  dt_data$asr <- dt_data$asr * dt_data$correction
  dt_data$st_err <- (dt_data$st_err / (total_pop^2))^(1/2)
  dt_data$st_err <- dt_data$st_err * dt_data$correction
  
  dt_data$asr <- round(dt_data$asr, digits = 2)
  dt_data$st_err <- round(dt_data$st_err, digits = 2)
  dt_data$correction <- round((dt_data$correction-1)*100, digits = 1)
  
  if (is.null(var_st_err)) {
    dt_data$st_err <- NULL
  } else {
    setnames(dt_data, "st_err", var_st_err)
  }
  
  if (var_asr!="asr") {
    setnames(dt_data, "asr", var_asr)
  }
  
  if (!correction_info) {
    dt_data$correction <- NULL
  }
  
  
  df_data <- data.frame(dt_data)
  
  
  
  if (bool_dum_age) {
    df_data$CSU_dum_age <- NULL
  }
  if (bool_dum_by) {
    df_data$CSU_dum_by <- NULL
  }
  
  setnames(df_data, "CSU_C", var_cases)
  setnames(df_data,  "CSU_P", var_py)
  
  
  if (is.null(pop_base_count)) {
    
    if (Rcan_print) {
      temp <- last_age*5-1
      if (last_age == 18)  temp <- "99+"
      cat("ASR have been computed for the age group ", (first_age-1)*5,"-", temp , "\n",  sep="" )
    }
    temp<- NULL
    
  } else {
    
    #cat("ASR have been computed for the age groups:\n",age_group_list , "\n",  sep="" )
    age_group_list<- NULL
    
  }
  
  return(df_data)
  
}