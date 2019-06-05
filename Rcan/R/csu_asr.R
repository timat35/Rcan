csu_asr <-
  function(df_data,
           var_age="age",
           var_cases="cases",
           var_py="py",
           group_by=NULL,
           var_age_group=NULL,
           missing_age = NULL,
           db_rate = 100000,
           first_age = 1,
           last_age = 18,
           pop_base = "SEGI",
           var_st_err=NULL,
           correction_info=FALSE,
           var_asr="asr",
           age_dropped = FALSE) 
  {
    
    core.error_variable(df_data, var_age, csu_asr)
    core.error_variable(df_data, var_cases, csu_asr)
    core.error_variable(df_data, var_py, csu_asr)
    
    if (first_age < 1 | first_age > 17 ) {
      stop('The argument "first_age" must be comprise between 1 (0-4) and 17 (80-85), see documentation: help(csu_asr)')
    }
    
    if (last_age < 2 | last_age > 18 ) {
      stop('The argument "last_age" must be comprise between 2 (5-9) and 18 (85+), see documentation: help(csu_asr)')
    }

    temp <- colnames(df_data)
    temp <- temp[!temp  %in% c(var_age,var_cases,var_py,group_by)]

    if (length(temp) > 0) {
        warning(paste0('The dataset variable: ',temp,' is not present in the group_by option.\n Cases data might have been summed while population data use the mean, please check carefully.\n\n'))
    }
    
    
    df_data <- core.csu_asr(df_data,var_age,var_cases,var_py,group_by,var_age_group,missing_age,db_rate,
                            first_age,last_age,pop_base,var_st_err,correction_info,var_asr,age_dropped, Rcan_print=TRUE)
    
    
    
    return(df_data)
    
  }

