

core.error_variable <- function(df_data, varname, funcname,type="numeric") {
  
  if (!is.null(varname)) {
  
    if (!(varname%in% colnames(df_data))) {
      
      stop(paste0(varname, " is not a column of ",   deparse(substitute(df_data)), " ,see documentation: Help(", deparse(substitute(funcname)), ")"))
      
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
}


core.error_time_variable <- function(df_data, var_year, group_by, funcname) {
  
  
  if (is.null(group_by)) {
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
  }
  
  dt_data <- data.table(df_data, key = c(group_by)) 
  dt_data$temp <- 1
  nrow_base <- nrow(dt_data)
  dt_test <- dt_data[ ,temp:=sum(temp), by=c(var_year, group_by)]
  nrow_test <-  nrow(dt_data[ ,sum(temp), by=c(var_year, group_by)]) 

  if (nrow_test != nrow_base) {
    setkeyv(dt_test, c(group_by,var_year))
    print(head(dt_test[temp>1, ]))
    dt_data <- NULL
    stop(paste0("There is more than 1 data per year (see above).\nUse the 'group_by' option"," ,see documentation: Help(", deparse(substitute(funcname)), ")\n","or call the function on a subset to define the sub-population of interest.\n"))
  }
}

core.csu_dt_rank <- function(df_data,
                        var_value = "CASES",
                        var_rank = "cancer_label",
                        group_by = NULL,
                        number = NULL, 
                        ties.method="min") {
  
  bool_dum_by <- FALSE
  if (is.null(group_by)) {
    
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
  }
  
  df_data <- as.data.table(df_data)
  dt_rank <- df_data[, list(rank_value=sum(get(var_value))), by=c(var_rank, group_by)]
  dt_rank[, CSU_RANK:= frank(-rank_value, ties.method=ties.method), by=group_by]
  
  if (!is.null(number)){
    dt_rank <- dt_rank[CSU_RANK <= number,c(group_by, var_rank, "CSU_RANK"), with=FALSE]
  }
  
  df_data <- merge(dt_rank, df_data,by=c(group_by, var_rank), all.x=TRUE)
  
  if (bool_dum_by) {
    
    df_data[,CSU_dum_by:=NULL]
    
  }
  
  return(df_data)
  
}


core.csu_legend_wrapper <- function(label, width) {
  
  label <- sapply(strwrap(label, width = width, simplify = FALSE), paste, collapse="\n")
  return(label)
  
}



core.csu_tick_generator <- function(max,min = 0,logscale=FALSE) {
  
  
  
  if (!logscale) {
    
    if (min > 0) {
      min = 0
    } 
    
    if (max < 0) {
      max = 0
    }
    
    
    temp_log_max = 10^floor(log10(max-min))
    temp_unit_floor_max = floor((max-min)/(temp_log_max))
    
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
    temp_floor <- floor(min/tick_space)*tick_space
    tick_list <- seq(temp_floor, temp_top, tick_space)
    tick_minor_list <- NULL
    
  } else {
    
    
    temp_log_max = 10^floor(log10(max))
    temp_unit_floor_max = floor(max/(temp_log_max))
    
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
  
  mod <- 5
  if (max - min < 10 ) {
    mod <- 1 
  } else if (max - min < 20){
    mod <- 2
  } 
  
  
  temp1 <- min - (min %% mod)
  temp2 <- max - (max %% mod) +ifelse(mod>=5,mod,0)
  
  if (temp2 - temp1 <= mod*6) {
    year_space <- mod 
    year_list <- seq(temp1,temp2,year_space)
    year_minor_list <- year_list
    
  } else  {
    year_space <- mod*2 
    if (temp1 %% mod*2 > 0) {
      year_list <- seq(temp1+mod,temp2,year_space)
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
  
  #format
  #if (!is.null(format_export)) {
  #  filename <- gsub("[[:punct:][:space:]\n]", "_", plot_title)
  #  core.csu_format_export(format_export, plot_title = filename, landscape = FALSE)
  #}
  
  # help file
  # \item{format_export}{
  #   export the graph in different format:\cr
  #   \tabular{ll}{
  #     \code{NULL} \tab Plot in R studio windows.\cr
  #     \code{"pdf"} \tab Export in PDF format.\cr
  #     \code{"tiff"} \tab Export in TIFF 300dpi format.\cr
  #     \code{"png"} \tab Export in PNG 200dpi format.\cr
  #     \code{"svg"} \tab Export in SVG format. Can be edit with \url{https://inkscape.org/fr/}.\cr
  #   }
  #   The filename is the \code{plot_title} option.
  # }
  # \item{graph_dev}{
  #   If the plot is embedded in a graphics Device function (such as pdf()), the graph_dev option should be set to TRUE for the first graph to avoid a blank page.
  # }
  

  
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
  dt_data[dt_data$CSU_A %in% missing_age,CSU_A:=NA ] 
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



core.csu_eapc <-
  function(df_data,
           var_rate="asr",
           var_year="year",
           group_by= NULL,
           var_eapc="eapc", 
					 CI_level = 0.95) {
    
    #create fake group to have group_by optional 
    bool_dum_by <- FALSE
    
    if (is.null(group_by)) {
      
      df_data$CSU_dum_by <- "dummy_by"
      group_by <- "CSU_dum_by"
      bool_dum_by <- TRUE
    }
    
    dt_data <- data.table(df_data, key = c(group_by)) 
    
    setnames(dt_data, var_rate, "CSU_R")
    setnames(dt_data, var_year, "CSU_Y")
    
    dt_data[, id_group:=.GRP, by=group_by]
    
    temp_max <- max(dt_data$id_group)
    for (i in 1:temp_max) {
      suppressWarnings(
        temp <- summary(glm(CSU_R ~ CSU_Y,
                            family=poisson(link="log"),
                            data=dt_data[dt_data$id_group  == i,] 
        )
        )
      )
      dt_data[dt_data$id_group  == i, CSU_EAPC:=temp$coefficients[[2]]]
      dt_data[dt_data$id_group  == i, CSU_ST:=temp$coefficients[[4]]]
      
    }
    
    dt_data$CSU_UP <- 100*(exp(dt_data$CSU_EAPC-(qt((1-CI_level)/2, df = Inf)*dt_data$CSU_ST))-1)
    dt_data$CSU_LOW <- 100*(exp(dt_data$CSU_EAPC+(qt((1-CI_level)/2, df = Inf)*dt_data$CSU_ST))-1)
    dt_data$CSU_EAPC <- 100*(exp(dt_data$CSU_EAPC)-1)
    
    
    
    dt_data<-  dt_data[,list( CSU_EAPC=mean(CSU_EAPC), CSU_UP=mean(CSU_UP),CSU_LOW=mean(CSU_LOW)), by=group_by]
    
    
    setnames(dt_data, "CSU_EAPC", var_eapc)
    setnames(dt_data, "CSU_UP", paste(var_eapc, "up", sep="_"))
    setnames(dt_data, "CSU_LOW", paste(var_eapc, "low", sep="_"))
    
    df_data <- data.frame(dt_data)
    if (bool_dum_by) {
      df_data$CSU_dum_by <- NULL
    }
    
    
  
    return(df_data)
    
    
    
}




core.csu_ageSpecific <-
  function(df_data,
           var_age="age",
           var_cases="cases",
           var_py="py",
           group_by = NULL,
           missing_age = NULL,
           db_rate = 100000,
           logscale=FALSE, 
           plot_title=NULL,
           legend=csu_trend_legend(),
           color_trend = NULL,
           CI5_comparison=NULL,
           linesize = 0.5,
           age_label_list = NULL,
           log_point=TRUE,
           plot_subtitle=NULL,
           plot_caption=NULL,
					 xtitle = "Age at diagnosis",
					 ytitle = "Age-specific incidence rate per",
					 label_group_by = waiver())

					 {
    
    
    
    bool_CI5_comp <- FALSE
    CI5_cancer_label <- NULL
    bool_dum_by <- FALSE
    
    
    # manage CI5 import
    if (!is.null(CI5_comparison)) {
      
      bool_CI5_comp <- TRUE
      data(csu_ci5x_mean, envir = e <- new.env())
      df_CI5 <- e$csu_ci5x_mean
      dt_CI5 <- data.table(df_CI5)
      if (is.character(CI5_comparison)) {
        if (!(CI5_comparison%in% dt_CI5$ci5_cancer_label)) {
          stop('CI5_comparison value must be a correct cancer label, see documentation: Help(CI5X_mean_data)')
          
        } else {
          dt_CI5 <- dt_CI5[dt_CI5$ci5_cancer_label == CI5_comparison, ]
        }
        
      } else {
        if (is.numeric(CI5_comparison)) {
          if (!(CI5_comparison%in% dt_CI5$ci5_cancer_code)) {
            stop('CI5_comparison value must be a correct cancer code, see documentation: Help(CI5X_mean_data)')
            
          } else {
            dt_CI5 <- dt_CI5[dt_CI5$ci5_cancer_code == CI5_comparison, ]
          }
        }
      }
      CI5_cancer_label <- toString(dt_CI5$ci5_cancer_label[1])
    }
    
    
    # manage group by options
    if (is.null(group_by)) {
      
      df_data$CSU_dum_by <- "dummy_by"
      group_by <- "CSU_dum_by"
      bool_dum_by <- TRUE
    }
    
    
    dt_data <- data.table(df_data, key = group_by)
    setnames(dt_data, var_age, "CSU_A")
    setnames(dt_data, var_cases, "CSU_C")
    setnames(dt_data, var_py, "CSU_P")
    setnames(dt_data, group_by, "CSU_BY")
    
    ##group population (use sum)
    dt_data <- dt_data[, list(CSU_C=sum(CSU_C),CSU_P=sum(CSU_P)), by=c("CSU_BY", "CSU_A") ]
    
    ##calcul rate 
    dt_data$rate <- dt_data$CSU_C/dt_data$CSU_P *db_rate
    
    ##change by to factor
    dt_data$CSU_BY <- factor(dt_data$CSU_BY)
    
    ##to calcul age group
    
    dt_data[CSU_A %in% missing_age,CSU_A:=NA ] 
    dt_data[is.na(CSU_A),CSU_P:=0 ] 
    dt_data <- dt_data[CSU_P!=0] 
    
    dt_data$CSU_age_factor <- c(as.factor(dt_data$CSU_A))
    dt_data[CSU_P != 0,nb_age_group := max(CSU_age_factor), by="CSU_BY"] 
    
    
    for (i in 15:17) {
      if (i %in% dt_data$nb_age_group) {
        dt_data[nb_age_group == i & CSU_age_factor >= i , CSU_C:=sum(CSU_C), by="CSU_BY"] ##add total_know
        dt_data[nb_age_group == i & CSU_age_factor > i & !is.na(CSU_age_factor), CSU_C := 0] 
      } 
    }
    ##create age label:
    if (is.null(age_label_list)) {
      
      ##create age dummy: 1 2 3 4 --- 18
      ##regroup case for population with nb of age group <  18 
      
      
      max_age <- max(dt_data$nb_age_group)
      
      if (max_age > 18) {
        stop('The function cannot have more than 18 age-group, see documentation: Help(csu_graph_ageSpecific)')
      }
      
      age_label <- c("0-4","5-9","10-14","15-19","20-24","25-39","30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
      
      age_label <- c(age_label[1:(max_age-1)], paste0((max_age-1)*5,"+"))
                     
    } else {
      age_label <-age_label_list
      max_age <- length(age_label)
    }
    
    ## to calcul breaks
    
    if (logscale) {
      min_tick_value <- min(dt_data[rate != 0,]$rate)
      
    } else {
      min_tick_value <- 0
    }
    
    
    tick <- core.csu_tick_generator(max = max(dt_data$rate), min=min_tick_value, logscale = logscale )
    tick_space <- tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list)-1]
    
    temp_top <- ceiling(max(dt_data$rate)/tick_space)*tick_space
    temp_expand_y <- max(dt_data$rate)/35
    temp_expand_y_up <- max(dt_data$rate)+temp_expand_y
    if (temp_expand_y_up > temp_top-(tick_space/2)) {
      temp_expand_y_up <- temp_top+temp_expand_y
    }
    
    th_legend <- list(theme(legend.position="none"))
    
    if (!bool_dum_by & legend$position == "bottom") {
      
      th_legend <- list(theme(
        legend.key = element_rect(fill="transparent"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size=unit(1,"cm"),
        legend.margin = margin(0, 0, 0, 0)
      ))
    }
    
    if (bool_CI5_comp & is.null(age_label_list)) {
      
      if (max_age < 18) {
        dt_CI5[CSU_age_factor >= max_age , CSU_C:=sum(CSU_C)] ##add total_know
        dt_CI5[ CSU_age_factor >= max_age , CSU_P:=sum(CSU_P)]
        dt_CI5 <- dt_CI5[CSU_age_factor <= max_age]    
      }
      

      
      dt_CI5$rate <- dt_CI5$CSU_C/dt_CI5$CSU_P *db_rate
      
      if (logscale) {
        dt_CI5[rate == 0 , rate:= NA]
      }
      
    }
    
    
    ylim_inf <- min(c(tick$tick_list, tick$tick_minor_list))
    ylim_sup <- max(c(tick$tick_list, tick$tick_minor_list))

    
    ##csu_plot
    if (logscale) {
      base_plot <- ggplot(dt_data[, rate := ifelse(rate==0,NA, rate )], aes(CSU_age_factor, rate))
    } else {
      base_plot <- ggplot(dt_data, aes(CSU_age_factor, rate))
    }
    if (bool_CI5_comp) {
      
      pos_y_text = - tick_space
      if (temp_top/tick_space > 7) {
        
        pos_y_text = pos_y_text*1.5
        
      }
      
      if(is.null(plot_caption)) {
        plot_caption <- paste0("- - - - - - : Mean for ",CI5_cancer_label," cancer in CI5 X")
      }
      
      
      base_plot <- base_plot + 
        geom_line(data = dt_CI5,
                  size = 1,
                  linetype=2,
                  colour = "grey50", 
                  show.legend=FALSE)
      
    } 
    
    
    csu_plot <- base_plot+
      geom_line(aes(color=CSU_BY), size = 1,na.rm=TRUE)+
      guides(color = guide_legend(override.aes = list(size=0.75)))+
      labs(title = plot_title,
           subtitle = plot_subtitle,
           caption = plot_caption)+
      scale_x_continuous(name = xtitle,
                         breaks=seq(1, max_age, 1),
                         labels = age_label,
                         minor_breaks = NULL,
                         expand = c(0.015,0.015)
      )
    
    if (logscale){
      if (log_point) {
        csu_plot <- csu_plot +
          geom_point(aes(fill=CSU_BY), size = 3,na.rm=TRUE,shape=21,stroke=0.5,colour="black", show.legend=FALSE)
      }
      csu_plot <- csu_plot +
        scale_y_continuous(name = paste(ytitle, formatC(db_rate, format="d", big.mark=",")),
                           breaks=tick$tick_list,
                           minor_breaks = tick$tick_minor_list,
                           limits=c(ylim_inf,ylim_sup),
                           labels=core.csu_axes_label,
                           trans = "log10"
        )
    } else {
      
      csu_plot <- csu_plot +
        coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
        scale_y_continuous(name = paste(ytitle, formatC(db_rate, format="d", big.mark=",")),
                           breaks=tick$tick_list,
                           labels=core.csu_axes_label,
                           expand = c(0,0)
        )
    } 
    
    
    
    csu_plot <- csu_plot +
      theme(
        plot.background= element_blank(),
        panel.background = element_blank(),
        panel.grid.major= element_line(colour = "grey70"),
        panel.grid.minor= element_line(colour = "grey70"),
        plot.title = element_text(size=16, margin=margin(0,0,15,0),hjust = 0.5),
        plot.subtitle = element_text(size=15, margin=margin(0,0,15,0),hjust = 0.5),
        plot.caption = element_text(size=10, margin=margin(15,0,0,0)),
        axis.title = element_text(size=14),
        axis.title.y = element_text(margin=margin(0,15,0,0)),
        axis.title.x = element_text(margin=margin(15,0,0,0)),
        plot.margin=margin(20,20,20,20),
        axis.text = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, angle = 60,  hjust = 1),
        axis.ticks= element_line(colour = "black", size = linesize),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line.x = element_line(colour = "black", 
                                   size = linesize, linetype = "solid"),
        axis.line.y = element_line(colour = "black", 
                                   size = linesize, linetype = "solid")
      )+
      th_legend
		
			
    
    if (!is.null(color_trend)) {
      
      csu_plot <- csu_plot +
        scale_colour_manual(name=legend$title,
														labels = label_group_by,
                            values= color_trend,
                            drop = FALSE)
      
      if (logscale) {
        csu_plot <- csu_plot +
          scale_fill_manual(labels = label_group_by,
														values= color_trend,
                            drop = FALSE)
      }
      
      
    } else {
      csu_plot <- csu_plot +
        scale_colour_discrete(name=legend$title)
    }
    
    if (!bool_dum_by & legend$position=="right") {
      
      csu_plot <- csu_plot + 
        geom_text(data = dt_data[CSU_age_factor == nb_age_group, ],
                  aes(label = CSU_BY),
                  hjust=-0.05)+
        theme(plot.margin = unit(c(0.5, legend$right_space_margin, 0.5, 0.5), "lines"))
      
    } else {
      
      csu_plot <- csu_plot +
        guides(color = guide_legend(nrow=legend$nrow))
    }
    
    
    
    dt_data$nb_age_group <- NULL
    dt_data$CSU_age_factor <- NULL
    
    if (logscale){
      dt_data[, rate := ifelse(is.na(rate),0, rate )]
    }
    
    
    return(list(csu_plot = csu_plot, dt_data = dt_data, CI5_cancer_label = CI5_cancer_label,legend_position=legend$position,bool_dum_by = bool_dum_by))
    
  }



core.csu_ageSpecific_top <- function(df_data, 
                                     var_age, 
                                     var_cases, 
                                     var_py,
                                     var_top,
                                     group_by=NULL,
                                     missing_age=NULL,
                                     db_rate = 100000,
                                     logscale = FALSE,
                                     nb_top = 5,
                                     plot_title=NULL,
                                     plot_subtitle=NULL,
																		 label_group_by=NULL,
																		 xtitle = "Age at diagnosis",
																		 ytitle = "Age-specific incidence rate per",
                                     var_color=NULL,
                                     plot_caption=NULL,
                                     var_age_label_list = NULL,
                                     caption_bypass=FALSE) {
  
  
  
  
  bool_dum_by <- FALSE
  
  if (is.null(group_by)) {
    
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    df_data$CSU_dum_by <- factor(df_data[[group_by]],levels=c("dummy_by"), labels=c(""))
    bool_dum_by <- TRUE
  } else {
    df_data$CSU_dum_by <- as.factor(df_data[[group_by]])
  }
  
  df_data <- core.csu_dt_rank(df_data, var_value = var_cases, var_rank = var_top,group_by = group_by, number = nb_top) 
  df_data[["dummy_top"]] <-core.csu_legend_wrapper(df_data[[var_top]], 14)
  
  
  plotlist <- list()
  datalist <- list()
  j <- 1 
  
  #dummmy variable to factorize variable
  df_data$CSU_dum_by <- as.factor(df_data[[group_by]])
  for (i in levels( df_data$CSU_dum_by)) {
    
		if (!is.null(label_group_by)) {
		
				label_group <- label_group_by[j]
		}
		else {
			label_group <- i
		}
		
    if (caption_bypass) {
      if (j == 1) {
        plot_caption <- ""
      } else {
        plot_caption <- plot_title
        plot_title <- ""
  
      }
    }
    
    dt_plot <- df_data[get("CSU_dum_by") == i]
    
    if (!is.null(var_color)) {
      dt_label_order <- setkey(unique(dt_plot[, c("dummy_top",var_color, "CSU_RANK"), with=FALSE]), CSU_RANK)
      dt_plot$dummy_top <- factor(dt_plot$dummy_top,levels = dt_label_order$dummy_top) 
      color_trend <- as.character(dt_label_order[[var_color]])
    } else {
      dt_label_order <- setkey(unique(dt_plot[, c(var_top, "CSU_RANK"), with=FALSE]), CSU_RANK)
      dt_plot[[var_top]] <- factor(dt_plot[[var_top]],levels = dt_label_order[[var_top]]) 
      color_trend <- NULL
      
    }
    
    
    if (!is.null(var_age_label_list)) {
      age_label_list <- unique(dt_plot[[var_age_label_list]])
    } else {
      age_label_list <- NULL
    }
    
    if (is.null(plot_subtitle)) {
      subtitle_temp <- label_group
    } else {
      subtitle_temp <- paste0(plot_subtitle,"\n",label_group)
    }
    
   
    
    temp <- core.csu_ageSpecific(
      dt_plot,
      var_age=var_age,
      var_cases= var_cases,
      var_py=var_py,
      group_by = "dummy_top",
      missing_age = missing_age,
      db_rate= db_rate,
      plot_title = plot_title,
      plot_subtitle = subtitle_temp,
      plot_caption = plot_caption,
      color_trend = color_trend,
      logscale = logscale,
      log_point=FALSE,
      age_label_list = age_label_list,
			xtitle = xtitle,
			ytitle = ytitle
      )
    
    dt_temp <- temp$dt_data
    dt_temp[[group_by]] <- i

    
    setnames(dt_temp, "CSU_BY", "dummy_top")
    
    

    
    plotlist[[j]] <- temp$csu_plot
    datalist[[j]] <- dt_temp
    
    j <- j+1
  }
  
  df_data <- unique(df_data[,c("dummy_top",group_by, var_top, "CSU_RANK"), with=FALSE])
  
  dt_final <- NULL
  for (i in 1:(j-1)) {
    dt_final <- rbind(dt_final, datalist[[i]])
  }
  
  dt_final <-  merge(dt_final,df_data,by=c("dummy_top",group_by),all.x=TRUE, all.y=FALSE)
  dt_final[, dummy_top :=NULL]
  setnames(dt_final, "CSU_A", var_age)
  setnames(dt_final, "CSU_C", var_cases)
  setnames(dt_final, "CSU_P", var_py)
  
  setkeyv(dt_final, c(group_by,"CSU_RANK",var_age))
  
  if (bool_dum_by) {
    dt_final[, CSU_dum_by:=NULL]
  }
  
  return(list(plotlist=plotlist, dt_data=dt_final))
  
}



core.csu_time_trend <- function (
  df_data,
  var_trend = "asr",
  var_year = "year",
  group_by = NULL,
  logscale = TRUE,
  smoothing = NULL,
  legend = csu_trend_legend(),
  color_trend = NULL,
  ytitle = "Age standardized rate per 100000",
	xtitle = "Year",
  plot_title = "test",
  linesize = 0.5,
  plot_subtitle = NULL,
  plot_caption = NULL) {
  
  
  if (!is.null(smoothing)) {
    if (smoothing == 0) {
      smoothing <- NULL
    }
  }
  
  
  
  dt_data <- data.table(df_data, key = group_by)
  setnames(dt_data, var_year, "CSU_Y")
  setnames(dt_data, var_trend, "CSU_T")
  setnames(dt_data, group_by, "CSU_BY")
  
  bool_dum_by <- FALSE
  if (is.null(group_by)) {
    
    dt_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
  }
  
  #change by to factor
  dt_data$CSU_BY <- factor(dt_data$CSU_BY)
  
  #smooth with loess  fonction
  if (!is.null(smoothing))
  {
    dt_data[,CSU_smooth:= loess( CSU_T ~ CSU_Y, span=smoothing)$fitted, by=CSU_BY]
  } else {
    dt_data[,CSU_smooth:= CSU_T]
  }
  
  dt_data[, max_year:=max(CSU_Y), by=CSU_BY]
  
  # to calcul y axes breaks
  
  if (logscale) {
    min_tick_value <- min(dt_data[CSU_smooth > 0,]$CSU_smooth)
  } else {
    min_tick_value <- 0
  }
  
  
  tick <- core.csu_tick_generator(max = max(dt_data$CSU_smooth), min=min_tick_value, logscale = logscale )
  tick_space <- tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list)-1]
  
  
  #to calcul year axes break
  
  year_tick <-  core.csu_year_tick_generator(min(dt_data$CSU_Y),max(dt_data$CSU_Y))
  
  
  
  
  temp_top <- ceiling(max(dt_data$CSU_smooth)/tick_space)*tick_space
  temp_expand_y <- max(dt_data$CSU_smooth)/35
  temp_expand_y_up <- max(dt_data$CSU_smooth)+temp_expand_y
  if (temp_expand_y_up > temp_top-(tick_space/2)) {
    temp_expand_y_up <- temp_top+temp_expand_y
  }
  
  th_legend <- list(theme(legend.position="none"))
  
  if (!bool_dum_by & legend$position == "bottom") {
    
    th_legend <- list(theme(
      legend.key = element_rect(fill="transparent"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.key.size=unit(1,"cm"),
      legend.margin = margin(0, 0, 0, 0)
    ))
  }
  
  
  xlim_inf <- min(c(year_tick$tick_list, year_tick$tick_minor_list))
  xlim_sup <- max(c(year_tick$tick_list, year_tick$tick_minor_list))
  
  ylim_inf <- min(c(tick$tick_list, tick$tick_minor_list))
  ylim_sup <- max(c(tick$tick_list, tick$tick_minor_list))

  
  #csu_plot
  if (logscale) {
    base_plot <- ggplot(dt_data[, CSU_smooth := ifelse(CSU_smooth==0,NA, CSU_smooth )], aes(CSU_Y, CSU_smooth))
  } else {
    base_plot <- ggplot(dt_data, aes(CSU_Y, CSU_smooth))
  }
  
  csu_plot <- base_plot+
    geom_line(aes(color=CSU_BY), size = 0.75,na.rm=TRUE)+
    guides(color = guide_legend(override.aes = list(size=0.75)))+
    labs(title = plot_title, 
         subtitle = plot_subtitle,
         caption = plot_caption)+
    scale_x_continuous(name = xtitle,
                       breaks=year_tick$tick_list,
                       limits=c(xlim_inf,xlim_sup),
                       minor_breaks = year_tick$tick_minor_list,
                       expand = c(0.015,0.015)
    )
  
  
  
  if (logscale){
    
    
    csu_plot <- csu_plot +
      scale_y_continuous(name = ytitle,
                         breaks=tick$tick_list,
                         minor_breaks = tick$tick_minor_list,
                         limits=c(ylim_inf,ylim_sup),
                         labels=core.csu_axes_label,
                         trans = "log10"
      )
  } else {
    
    csu_plot <- csu_plot +
      coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
      scale_y_continuous(name = ytitle,
                         breaks=tick$tick_list,
                         labels=core.csu_axes_label,
                         expand = c(0,0)
      )
  } 
  
  if (is.null(color_trend)) {
    csu_plot <- csu_plot +scale_colour_discrete(name=legend$title)
  } 
  else {
    csu_plot <- csu_plot +scale_colour_manual(name=NULL, values=color_trend)
  }
  
  
  csu_plot <- csu_plot +
    theme(
      plot.background= element_blank(),
      panel.background = element_blank(),
      panel.grid.major= element_line(colour = "grey70"),
      panel.grid.minor= element_line(colour = "grey70"),
      plot.title = element_text(size=16, margin=margin(0,0,15,0),hjust = 0.5),
      plot.subtitle = element_text(size=15, margin=margin(0,0,15,0),hjust = 0.5),
      plot.caption = element_text(size=10, margin=margin(15,0,0,0)),
      axis.title = element_text(size=12),
      axis.title.y = element_text(margin=margin(0,15,0,0)),
      axis.title.x = element_text(margin=margin(15,0,0,0)),
      plot.margin=margin(20,20,20,20),
      axis.text = element_text(size=12, colour = "black"),
      axis.text.x = element_text(size=12,  hjust = 0.5),
      axis.ticks= element_line(colour = "black", size = linesize),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.x = element_line(colour = "black", 
                                 size = linesize, linetype = "solid"),
      axis.line.y = element_line(colour = "black", 
                                 size = linesize, linetype = "solid")
    )+
    th_legend
  
  
  
  if (!bool_dum_by & legend$position=="right") {
    
    csu_plot <- csu_plot + 
      geom_text(data = dt_data[CSU_Y == max_year, ],
                aes(label = CSU_BY),
                hjust=0,
                nudge_x=0.5)+
      theme(plot.margin = unit(c(0.5, legend$right_space_margin, 0.5, 0.5), "lines"))
    
  } else {
    
    csu_plot <- csu_plot +
      guides(color = guide_legend(nrow=legend$nrow))
  }
  
  return(list(dt_data=dt_data, csu_plot = csu_plot, legend_position=legend$position,bool_dum_by = bool_dum_by))
  
  
}



