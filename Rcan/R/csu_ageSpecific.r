

csu_ageSpecific <-
  function(df_data,var_age="age", var_cases="cases", var_py="py", var_by = NULL, missing_age = NULL,db_rate = 100000,  plot_title="csu_title", legend=csu_trend_legend(), format_export=NULL, draw_plot=TRUE, return_rate=FALSE, var_rate="rate", CI5_comparaison=NULL) {


    
    if (!(legend$position %in% c("bottom", "right"))) {
      
      stop('legend position must be "bottom" or "right", see documentation: Help(csu_ageSpecific)')
      
    }
    
    if (legend$right_space_margin > 20) {
      
      stop('legend right space margin must be < 20, see documentation: Help(csu_ageSpecific)')
      
    }

    
    if (!(var_age%in% colnames(df_data))) {
      
      stop('var_age value is not a variable name of the data, see documentation: Help(csu_ageSpecific)')
      
    }
    
    if (!(var_cases%in% colnames(df_data))) {
      
      stop('var_cases value is not a variable name of the data, see documentation: Help(csu_ageSpecific)')
      
    }
    
    if (!(var_py%in% colnames(df_data))) {
      
      stop('var_py value is not a variable name of the data, see documentation: Help(csu_ageSpecific)')
      
    }

    bool_CI5_comp <- FALSE
    
    if (!is.null(CI5_comparaison)) {
      
      bool_CI5_comp <- TRUE
	  data(csu_ci5x_mean, envir = e <- new.env())
    df_CI5 <- e$csu_ci5x_mean
      dt_CI5 <- data.table(df_CI5)
      if (is.character(CI5_comparaison)) {
          if (!(CI5_comparaison%in% dt_CI5$ci5_cancer_label)) {
          stop('CI5_comparaison value must be a correct cancer label, see documentation: Help(CI5X_mean_data)')
          
        } else {
          dt_CI5 <- dt_CI5[dt_CI5$ci5_cancer_label == CI5_comparaison, ]
        }
        
      } else {
        if (is.numeric(CI5_comparaison)) {
          if (!(CI5_comparaison%in% dt_CI5$ci5_cancer_code)) {
            stop('CI5_comparaison value must be a correct cancer code, see documentation: Help(CI5X_mean_data)')
            
          } else {
            dt_CI5 <- dt_CI5[dt_CI5$ci5_cancer_code == CI5_comparaison, ]
          }
        }
      }
      CI5_cancer_label <- toString(dt_CI5$ci5_cancer_label[1])
    }
    
    bool_dum_by <- FALSE
    
    if (is.null(var_by)) {
      
      df_data$CSU_dum_by <- "dummy_by"
      var_by <- "CSU_dum_by"
      bool_dum_by <- TRUE
    }
    
    
    if ( length(var_by) > 1) {
      
      stop('Only one variable can be use in the "var_by" option, see documentation: Help(csu_ageSpecific)')
      
    }
    
    if (!(var_by%in% colnames(df_data))) {
      
      stop('var_by value is not a variable name of the data, see documentation: Help(csu_ageSpecific)')
      
    }
    
    dt_data <- data.table(df_data, key = var_by)
    setnames(dt_data, var_age, "CSU_A")
    setnames(dt_data, var_cases, "CSU_C")
    setnames(dt_data, var_py, "CSU_P")
    setnames(dt_data, var_by, "CSU_BY")
    
    temp <- dt_data[, lapply(.SD, function(x) is.numeric(x)) ]
    
    if (!temp[["CSU_A"]]) {
      
      stop('The variable "age" must be numeric, see documentation:  Help(csu_ageSpecific)')
      
    }
    
    if (!temp[["CSU_P"]]) {
      
      stop('The variable "population" must be numeric, see documentation:  Help(csu_ageSpecific)')
      
    }
    
    if (!temp[["CSU_C"]]) {
      
      stop('The variable "age" must be numeric, see documentation:  Help(csu_ageSpecific)')
      
    }
    
    if (!is.null(missing_age)) {
      if (!(missing_age %in% dt_data$CSU_A)) {
        stop('missing_age is not in the age value, see documentation: Help(csu_ageSpecific)')
      }
    }
    
    
    
    
    
    
    #group population (use sum)
    dt_data <- dt_data[, list(CSU_C=sum(CSU_C),CSU_P=sum(CSU_P)), by=c("CSU_BY", "CSU_A") ]
    
    #calcul rate 
    dt_data$rate <- dt_data$CSU_C/dt_data$CSU_P *db_rate
    
    #change by to factor
    dt_data$CSU_BY <- factor(dt_data$CSU_BY)
    
    #to calcul age group
    
    dt_data[dt_data$CSU_A==missing_age,CSU_A:=NA ] 
    dt_data[is.na(dt_data$CSU_A),CSU_P:=0 ] 
    dt_data <- dt_data[dt_data$CSU_P!=0] 
    
    #create age label:
    
    #create age dummy: 1 2 3 4 --- 18
    dt_data$CSU_age_factor <- c(as.factor(dt_data$CSU_A))
    dt_data[dt_data$CSU_P != 0,nb_age_group := max(CSU_age_factor), by="CSU_BY"] 
    
    
    #regroup case for population with nb of age group <  18 
    for (i in 15:17) {
      if (i %in% dt_data$nb_age_group) {
        dt_data[dt_data$nb_age_group == i & dt_data$CSU_age_factor >= i , CSU_C:=sum(CSU_C), by="CSU_BY"] #add total_know
        dt_data[dt_data$nb_age_group == i & dt_data$CSU_age_factor > i & !is.na(dt_data$CSU_age_factor), CSU_C := 0] 
      } 
    }
    
    max_age <- max(dt_data$nb_age_group)
    age_label <- c("0-4","5-9","10-14","15-19","20-24","25-39","30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
    
    if (max_age  < 18 ) {
      age_label <- c(age_label[1:16],"80+") 
      if (max_age  < 17) {
        age_label <- c(age_label[1:15],"75+") 
        if (max_age  < 16) {
          age_label <- c(age_label[1:14],"70+") 
          if (max_age  == 15) {
            age_label <- c(age_label[1:14],"65+") 
          } else {
            stop('The data need at least 15 age-group, see documentation: Help(csu_graph_ageSpecific)')
          }
        }
      }
    } else {
      if (max_age > 18) {
        stop('The function cannot have more than 18 age-group, see documentation: Help(csu_graph_ageSpecific)')
      }
    }
    
    
    # to calcul breaks
    temp_max <- max(dt_data$rate)
    temp_log = 10^floor(log10(temp_max))
    temp_unit_floor = floor(temp_max/(temp_log))
    
    if (temp_unit_floor < 2) {
      temp_tick = 0.2*temp_log
    } else {
      if (temp_unit_floor < 5) {
        temp_tick = 0.5*temp_log
      } else {
        temp_tick = temp_log
      }
    }
    
    
    
    
    temp_top <- ceiling(temp_max/temp_tick)*temp_tick
    temp_expand_y <- temp_max/35
    temp_expand_y_up <- temp_max+temp_expand_y
    if (temp_expand_y_up > temp_top-(temp_tick/2)) {
      temp_expand_y_up <- temp_top+temp_expand_y
    }
    
    
    th_legend <- list(theme(legend.position="none"))
    
    if (!bool_dum_by & legend$position == "bottom") {
      
      th_legend <- list(theme(
        legend.key = element_rect(fill="transparent"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      ))
    }
    
    if (bool_CI5_comp) {
      
      if (max_age < 18) {
        dt_CI5[dt_CI5$CSU_age_factor >= max_age , CSU_C:=sum(CSU_C)] #add total_know
        dt_CI5[ dt_CI5$CSU_age_factor >= max_age , CSU_P:=sum(CSU_P)]
        dt_CI5 <- dt_CI5[dt_CI5$CSU_age_factor <= max_age]    
      }
      
      dt_CI5$rate <- dt_CI5$CSU_C/dt_CI5$CSU_P *db_rate
      
    }
    
    
    #format
    if (!is.null(format_export)) {
      if (format_export == "pdf") {
        
        pdf(paste(plot_title,".pdf", sep=""))
        
      } else {
        if (format_export == "svg") {
          svg(paste(plot_title,".svg", sep=""))
        }
      }
    }
    
    
    
    
    #plot
    base_plot <- ggplot(dt_data, aes(CSU_age_factor, rate))
    
    if (bool_CI5_comp) {
      
      pos_y_text = - temp_tick
      if (temp_top/temp_tick > 7) {
        
        pos_y_text = pos_y_text*1.5
        
      }
      
      str_CI5 <- textGrob("- - - - - - : CI5 X", gp=gpar(fontsize=11, col = "grey30"))
      
      base_plot <- base_plot + 
        geom_line(data = dt_CI5,
                  size = 1,
                  linetype=2,
                  colour = "grey50", 
                  show.legend=FALSE)+
        annotation_custom(str_CI5,xmin=max_age-2,xmax=max_age-2,ymin=pos_y_text,ymax=pos_y_text)
      
    } 
    
    
    plot <- base_plot+
      geom_line(aes(color=CSU_BY), size = 1)+
      ggtitle(plot_title)+
      coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+ 
      scale_y_continuous(name = paste("Age-specific incidence rate per", formatC(db_rate, format="d")),
                         breaks=seq(0, temp_top, temp_tick),
                         expand = c(0,0)
      )+
      scale_x_continuous(name = "Age at diagnosis",
                         breaks=seq(1, max_age, 1),
                         labels = age_label,
                         minor_breaks = NULL,
                         expand = c(0.015,0.015)
      )+
      scale_colour_discrete(name=legend$title)+
      theme(
        plot.background= element_blank(),
        panel.background = element_blank(),
        panel.grid.major= element_line(colour = "grey70"),
        panel.grid.minor= element_line(colour = "grey70"),
        plot.title = element_text(size=16, margin=margin(0,0,15,0)),
        axis.title = element_text(size=12),
        axis.title.y = element_text(margin=margin(0,15,0,0)),
        axis.title.x = element_text(margin=margin(15,0,0,0)),
        axis.text = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=12, angle = 60,  hjust = 1),
        axis.ticks= element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line.x = element_line(colour = "black", 
                                   size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", 
                                   size = 0.5, linetype = "solid")
      )+
      th_legend
    
    
    
    if (draw_plot) {
      if (!bool_dum_by & legend$position=="right") {
        plot <- plot + 
          geom_text(data = dt_data[dt_data$CSU_A == nb_age_group, ],
                    aes(label = CSU_BY),
                    hjust=-0.05)+
          theme(plot.margin = unit(c(0.5, legend$right_space_margin, 0.5, 0.5), "lines"))
      } else {
        plot <- plot +
          guides(color = guide_legend(nrow=legend$nrow))
      }
      gb_plot <- ggplot_build(plot)
      gt_plot <- ggplot_gtable(gb_plot)
      gt_plot$layout$clip[gt_plot$layout$name=="panel"] <- "off"
      if(is.null(format_export)) {
        plot.new()
      }
      grid.draw(gt_plot)
      
      if (bool_CI5_comp) {
        cat("the dotted grey line represente the mean for ", CI5_cancer_label, " cancer in CI5 X\n", sep="")

      }
    }
    if (!is.null(format_export)) {
      dev.off()
    }
    
    
    if (var_rate!="rate") {
      setnames(dt_data, "rate", var_rate)
    }
    
    if (bool_dum_by) {
      df_data$CSU_dum_by <- NULL
    }
    
    dt_data$nb_age_group <- NULL
    dt_data$CSU_age_factor <- NULL
    
    setorder(dt_data,CSU_BY,CSU_A)
    df_data <- data.frame(dt_data)
    
    setnames(df_data, "CSU_A", var_age)
    setnames(df_data, "CSU_C", var_cases)
    setnames(df_data,  "CSU_P", var_py)
    setnames(df_data,  "CSU_BY", var_by)
    

    
  if (return_rate) {
    return(df_data)
  }
}
