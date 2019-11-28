csu_ageSpecific_top <-
  
  function(df_data,
           var_age="age",
           var_cases="cases",
           var_py="py",
           var_top, 
           group_by=NULL,
           missing_age=NULL,
           db_rate = 100000,
           logscale = FALSE,
           nb_top = 5,
           plot_title=NULL,
           plot_subtitle=NULL,
           var_color=NULL)
{


  core.error_variable(df_data, var_cases, csu_ageSpecific_top)
  core.error_variable(df_data, var_py, csu_ageSpecific_top)
  core.error_variable(df_data, var_top, csu_ageSpecific_top, type="")
  core.error_variable(df_data, group_by, csu_ageSpecific_top, type="")
  core.error_variable(df_data, var_color, csu_ageSpecific_top, type="")

           
  
  
    #call core function 
	temp <- core.csu_ageSpecific_top(df_data,var_age, var_cases, var_py,var_top, group_by,
	                                       missing_age, db_rate, logscale, nb_top, 
	                                       plot_title, plot_subtitle, var_color =var_color)

    
	for (i in  1:length(temp$plotlist)) {
	  
	  print(temp$plotlist[[i]]+guides(color = guide_legend(override.aes = list(size=1), nrow=1,byrow=TRUE)))

	}

    
    return(temp$dt_data)
    
  }

