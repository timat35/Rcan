csu_cumrisk <-
  function(df_data,
           var_age="age",
           var_cases="cases",
           var_py="py",
           group_by=NULL,
           missing_age = NULL,
           last_age = 15,
           var_st_err=NULL,
           correction_info=FALSE,
           var_cumrisk="cumrisk") 
  {
    
    core.error_variable(df_data, var_cases, csu_cumrisk)
    core.error_variable(df_data, var_py, csu_cumrisk)

    if (!is.null(missing_age)) {
      core.error_missingage(df_data, var_age,missing_age, csu_cumrisk)
    }

    core.error_age_parse(df_data, var_age, missing_age, csu_cumrisk)
    
    temp <- colnames(df_data)
    temp <- temp[!temp  %in% c(var_age,var_cases,var_py,group_by)]

    if (length(temp) > 0) {
        warning(paste0('The dataset variable: ',temp,' is not present in the group_by option.\n Cases and population data have been summed by the group_by variable, please check carefully.\n\n'))
    }
    
    
    df_data <- core.csu_cumrisk(df_data,var_age,var_cases,var_py,group_by,missing_age,
                          last_age,var_st_err,correction_info,var_cumrisk, Rcan_print=TRUE)

    
    
    return(df_data)
    
  }

