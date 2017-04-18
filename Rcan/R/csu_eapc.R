csu_eapc <-
  function(df_data,var_rate, var_period, var_by= NULL,var_eapc="eapc") {
  
    if (!(var_rate%in% colnames(df_data))) {
      
      stop('var_rate value is not a variable name of the data, see documentation: Help(csu_eapc)')
      
    }
    
    if (!(var_period%in% colnames(df_data))) {
      
      stop('var_period value is not a variable name of the data, see documentation: Help(csu_eapc)')
      
    }
    
    #create fake group to have var_by optional 
    bool_dum <- FALSE
    if (is.null(var_by)) {
      
      df_data$CSU_dum <- "dummy"
      var_by <- "CSU_dum"
      bool_dum <- TRUE
    }
    
    dt_data <- data.table(df_data, key = c(var_by)) 
    
    setnames(dt_data, var_rate, "CSU_R")
    setnames(dt_data, var_period, "CSU_P")
    
    #check by variable adapted (ie: 1 year per variable)
    dt_data$temp <- 1
    nrow_base <- nrow(dt_data)
    nrow_test <-  nrow(dt_data[ ,sum(temp), by=c("CSU_P",  var_by)]) 
    dt_data$temp <- NULL
    if (nrow_test != nrow_base) {
      dt_data <- NULL
      stop("There is more than 1 data per year.\nUse the option by to define the sub population.\n")
    }
    
    dt_data[, id_group:=.GRP, by=var_by]
    
    temp_max <- max(dt_data$id_group)
    for (i in 1:temp_max) {
      suppressWarnings(
        temp <- summary(glm(CSU_R ~ CSU_P,
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
    
    
    
    dt_data<-  dt_data[,list( CSU_EAPC=mean(CSU_EAPC), CSU_UP=mean(CSU_UP),CSU_LOW=mean(CSU_LOW)), by=var_by]
    
    
    setnames(dt_data, "CSU_EAPC", var_eapc)
    setnames(dt_data, "CSU_UP", paste(var_eapc, "up", sep="_"))
    setnames(dt_data, "CSU_LOW", paste(var_eapc, "low", sep="_"))
    
    df_data <- data.frame(dt_data)
    if (bool_dum) {
      df_data$CSU_dum <- NULL
    }
    
    cat("EAPC with standard errors have been computed\n" )
    
    return(df_data)
    
  }

