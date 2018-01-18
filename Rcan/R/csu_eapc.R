csu_eapc <-
  function(df_data,
  var_rate="asr",
  var_year="year",
  group_by= NULL,
  var_eapc="eapc") {
  
  
    core.error_variable(df_data, var_rate, csu_eapc)
	core.error_variable(df_data, var_year, csu_eapc)
	core.error_time_variable(df_data, var_year, group_by, csu_eapc)
  
    #call core function 
	df_data <- core.csu_eapc(df_data, var_rate, var_year, group_by, var_eapc)
    
	cat("EAPC with standard errors have been computed\n" )
    
    return(df_data)
    
  }

