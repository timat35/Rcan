csu_eapc <-
  function(df_data,
  var_rate="asr",
  var_year="year",
  group_by= NULL,
  var_eapc="eapc") {
  
    if (!(var_rate%in% colnames(df_data))) {
      
      stop('var_rate value is not a variable name of the data, see documentation: Help(csu_eapc)')
      
    }
    
    if (!(var_year%in% colnames(df_data))) {
      
      stop('var_year value is not a variable name of the data, see documentation: Help(csu_eapc)')
      
    }
    
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
    
    #check by variable adapted (ie: 1 year per variable)
    dt_data$temp <- 1
    nrow_base <- nrow(dt_data)
    dt_test <- dt_data[ ,temp:=sum(temp), by=c("CSU_Y", group_by)]
    nrow_test <-  nrow(dt_data[ ,sum(temp), by=c("CSU_Y", group_by)]) 
    dt_data$temp <- NULL
    
    if (nrow_test != nrow_base) {
      setkeyv(dt_test, c(group_by,"CSU_Y"))
      print(head(dt_test[temp>1, ]))
      dt_data <- NULL
      stop("There is more than 1 data per year (see above).\nUse the 'group_by' option or call the function on a subset to define the sub-population of interest.\n")
    }
    
    
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
    
    dt_data$CSU_UP <- 100*(exp(dt_data$CSU_EAPC+(1.65*dt_data$CSU_ST))-1)
    dt_data$CSU_LOW <- 100*(exp(dt_data$CSU_EAPC-(1.65*dt_data$CSU_ST))-1)
    dt_data$CSU_EAPC <- 100*(exp(dt_data$CSU_EAPC)-1)
    
    
    
    dt_data<-  dt_data[,list( CSU_EAPC=mean(CSU_EAPC), CSU_UP=mean(CSU_UP),CSU_LOW=mean(CSU_LOW)), by=group_by]
    
    
    setnames(dt_data, "CSU_EAPC", var_eapc)
    setnames(dt_data, "CSU_UP", paste(var_eapc, "up", sep="_"))
    setnames(dt_data, "CSU_LOW", paste(var_eapc, "low", sep="_"))
    
    df_data <- data.frame(dt_data)
    if (bool_dum_by) {
      df_data$CSU_dum_by <- NULL
    }
    
    cat("EAPC with standard errors have been computed\n" )
    
    return(df_data)
    
  }

