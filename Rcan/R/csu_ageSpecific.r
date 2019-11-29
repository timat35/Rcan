
csu_ageSpecific <- 
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
           var_rate="rate") {
    
    if (!(legend$position %in% c("bottom", "right"))) {
      
      stop('legend position must be "bottom" or "right", see documentation: Help(csu_ageSpecific)')
      
    }
    
    if (legend$right_space_margin > 20) {
      
      stop('legend right space margin must be < 20, see documentation: Help(csu_ageSpecific)')
      
    }
    
    core.error_variable(df_data, var_cases, csu_ageSpecific)
    core.error_variable(df_data, var_py, csu_ageSpecific)

    if (!is.null(missing_age)) {
      core.error_missingage(df_data, var_age,missing_age, csu_asr)
    }

    core.error_age_parse(df_data, var_age, missing_age, csu_asr)
    
    csu_list <- core.csu_ageSpecific(df_data,var_age, var_cases, var_py, group_by ,
                                     missing_age,db_rate,
                                     logscale, 
                                     plot_title,
                                     legend,
                                     color_trend,
                                     CI5_comparison,
                                     linesize=0.75)
    
    dt_data <- csu_list$dt_data
    

    if (csu_list$legend_position=="right") {

      gb_plot <- ggplot_build(csu_list$csu_plot)
      gt_plot <- ggplot_gtable(gb_plot)
      gt_plot$layout$clip[gt_plot$layout$name=="panel"] <- "off"
      grid.draw(gt_plot)
    } else {
      print(csu_list$csu_plot)
    }
    
    
    if (!is.null(csu_list$CI5_cancer_label)) {
      cat("the dotted grey line represente the mean for ", csu_list$CI5_cancer_label, " cancer in CI5 XI\n", sep="")
      
    }
    
    if (var_rate!="rate") {
      setnames(dt_data, "rate", var_rate)
    }
    
    setorder(dt_data,CSU_BY,CSU_A)
    df_data <- data.frame(dt_data)
    setnames(df_data, "CSU_A", var_age)
    setnames(df_data, "CSU_C", var_cases)
    setnames(df_data,  "CSU_P", var_py)
    
    if (!csu_list$bool_dum_by) {
      setnames(df_data,  "CSU_BY", group_by)
    } else {
      
      df_data$CSU_BY <- NULL
    }
    
    return(df_data)
    
  }


