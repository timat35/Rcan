csu_time_trend <- function (
  df_data,
  var_trend = "asr",
  var_year = "year",
  group_by = NULL,
  logscale = FALSE,
  smoothing = NULL,
  legend = csu_trend_legend(),
  color_trend = NULL,
  ytitle = "Age standardized rate per 100,000",
  plot_title = "csu_title") {
  
  linesize <- 0.75
  
  core.error_variable(df_data, var_trend, csu_time_trend)
  core.error_variable(df_data, var_year, csu_time_trend)
  core.error_time_variable(df_data, var_year, group_by, csu_time_trend)
  

  #check by variable adapted (ie: 1 year per variable)
  dt_data <- data.table(df_data, key = group_by)
  if (is.null(group_by)) {
    dt_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
  }
  
  dt_data$temp <- 1
  nrow_base <- nrow(dt_data)
  dt_test <- dt_data[ ,temp:=sum(temp), by=c(var_year,  group_by)]
  nrow_test <-  nrow(dt_data[ ,sum(temp), by=c(var_year,  group_by)]) 
  dt_data$temp <- NULL
  
  if (nrow_test != nrow_base) {
    setkeyv(dt_test, c(group_by,var_year))
    print(head(dt_test[temp>1, ]))
    dt_data <- NULL
    stop("There is more than 1 data per year (see above).\nUse the 'group_by' option or call the function on a subset to define the sub-population of interest.\n")
  }

  
 #call to core function
 csu_list <- core.csu_time_trend(dt_data,
                                 var_trend,
                                 var_year,
                                 group_by, 
                                 logscale,
                                 smoothing,
                                 legend,
                                 color_trend,
                                 ytitle,
                                 plot_title = plot_title,
                                 linesize = linesize) 
  
  dt_data <- csu_list$dt_data
  
  if (csu_list$legend_position=="right") {

	  gb_plot <- ggplot_build(csu_list$csu_plot)
	  gt_plot <- ggplot_gtable(gb_plot)
	  gt_plot$layout$clip[gt_plot$layout$name=="panel"] <- "off"
	  grid.draw(gt_plot) 
	
	} else {
      print(csu_list$csu_plot)
    }
	 
  if (is.null(smoothing)) {
	  dt_data[, CSU_smooth:=NULL]
  } else {
    setnames(dt_data, "CSU_smooth", "smooth_value")
  }
  
   dt_data[, max_year:=NULL]
   
  setorder(dt_data,CSU_BY,CSU_Y)
  df_data <- data.frame(dt_data)
  setnames(df_data, "CSU_Y", var_year)
  setnames(df_data, "CSU_T", var_trend)
  if (!csu_list$bool_dum_by) {
      setnames(df_data,  "CSU_BY", group_by)
    } else {
      
      df_data$CSU_BY <- NULL
    }
  return(df_data)
  
  
}



