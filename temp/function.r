


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
    
    
    Rcan:::core.error_variable(df_data, var_age, csu_ageSpecific)
    Rcan:::core.error_variable(df_data, var_cases, csu_ageSpecific)
    Rcan:::core.error_variable(df_data, var_py, csu_ageSpecific)
    
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
      cat("the dotted grey line represente the mean for ", csu_list$CI5_cancer_label, " cancer in CI5 X\n", sep="")
      
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






core.csu_ageSpecific <-function(df_data,
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

    #drop missing age 
    dt_data <- dt_data[CSU_A!=missing_age] 

  

    ##change by to factor
    dt_data$CSU_BY <- factor(dt_data$CSU_BY)

    ##to calcul age group
    dt_data[CSU_A %in% missing_age,CSU_A:=NA ] 
    dt_data[is.na(CSU_A),CSU_P:=0 ] 
    
    
    dt_data$CSU_age_factor <- c(as.factor(dt_data$CSU_A))
    dt_data <- merge(dt_data, dt_data[dt_data$CSU_P != 0,list(nb_age_group = max(CSU_age_factor)), by="CSU_BY"], by="CSU_BY")   

  
    for (i in 15:17) {
      if (i %in% dt_data$nb_age_group) {

        dt_data[nb_age_group == i & CSU_age_factor >= i , CSU_C:=sum(CSU_C), by="CSU_BY"] ##add total_know
        dt_data[nb_age_group == i & CSU_age_factor > i & !is.na(CSU_age_factor), CSU_C := 0] 
      } 
    }

    dt_data <- dt_data[CSU_P!=0] 
    ##calcul rate 
    dt_data$rate <- dt_data$CSU_C/dt_data$CSU_P *db_rate
  
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
    
    
    tick <- Rcan:::core.csu_tick_generator(max = max(dt_data$rate), min=min_tick_value, logscale = logscale )
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
                           labels=Rcan:::core.csu_axes_label,
                           trans = "log10"
        )
    } else {
      
      csu_plot <- csu_plot +
        coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
        scale_y_continuous(name = paste(ytitle, formatC(db_rate, format="d", big.mark=",")),
                           breaks=tick$tick_list,
                           labels=Rcan:::core.csu_axes_label,
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