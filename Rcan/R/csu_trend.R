csu_trend <- function (
  df_data,
  var_trend = "asr",
  var_year = "year",
  group_by = NULL,
  logscale = TRUE,
  smoothing = 0.3,
  legend = csu_trend_legend(),
  yaxes_title = "Age standardized rate per 100000",
  plot_title = "csu_title",
  format_export = NULL,
  graph_dev =FALSE) {
  
  linesize <- 0.75
  
  if (!is.null(smoothing)) {
	  if (smoothing == 0) {
	  smoothing <- NULL
	  }
  }
  
  bool_dum_by <- FALSE
  if (is.null(group_by)) {
    
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
  }
  

  
  dt_data <- data.table(df_data, key = group_by)
  setnames(dt_data, var_year, "CSU_Y")
  setnames(dt_data, var_trend, "CSU_T")
  setnames(dt_data, group_by, "CSU_BY")
  
  #check by variable adapted (ie: 1 year per variable)
  dt_data$temp <- 1
  nrow_base <- nrow(dt_data)
  dt_test <- dt_data[ ,temp:=sum(temp), by=c("CSU_Y",  "CSU_BY")]
  nrow_test <-  nrow(dt_data[ ,sum(temp), by=c("CSU_Y",  "CSU_BY")]) 
  dt_data$temp <- NULL
  
  if (nrow_test != nrow_base) {
    setkeyv(dt_test, c("CSU_BY","CSU_Y"))
    print(head(dt_test[temp>1, ]))
    dt_data <- NULL
    stop("There is more than 1 data per year (see above).\nUse the 'group_by' option or call the function on a subset to define the sub-population of interest.\n")
  }

  
  #change by to factor
  dt_data$CSU_BY <- factor(dt_data$CSU_BY)
  
  #smooth with loess  fonction
  if (!is.null(smoothing))
  {
    dt_data[,smooth_value:= loess( CSU_T ~ CSU_Y, span=smoothing)$fitted, by=CSU_BY]
  } else {
    dt_data[,smooth_value:= CSU_T]
  }
  
  dt_data[, max_year:=max(CSU_Y), by=CSU_BY]
  
  # to calcul y axes breaks
  tick <- .csu_tick_generator(max = max(dt_data$smooth_value), min=min(dt_data[smooth_value != 0,]$smooth_value), log_scale = logscale )
  tick_space <- tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list)-1]
  
  
  #to calcul year axes break
  year_tick <- .csu_year_tick_generator(min(dt_data$CSU_Y),max(dt_data$CSU_Y))

  
  
  temp_top <- ceiling(max(dt_data$smooth_value)/tick_space)*tick_space
  temp_expand_y <- max(dt_data$smooth_value)/35
  temp_expand_y_up <- max(dt_data$smooth_value)+temp_expand_y
  if (temp_expand_y_up > temp_top-(tick_space/2)) {
    temp_expand_y_up <- temp_top+temp_expand_y
  }
  
  th_legend <- list(theme(legend.position="none"))
  
  if (!bool_dum_by & legend$position == "bottom") {
    
    th_legend <- list(theme(
      legend.key = element_rect(fill="transparent"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.key.size=unit(1,"cm"),
      legend.margin = margin(0, 0, 0, 0)
    ))
  }
  
  #format
  if (!is.null(format_export)) {
    filename <- gsub("[[:punct:][:space:]\n]", "_", plot_title)
    .csu_format_export(format_export, plot_title = filename, landscape = FALSE)
  }
  
  xlim_inf <- min(c(year_tick$tick_list, year_tick$tick_minor_list))
  xlim_sup <- max(c(year_tick$tick_list, year_tick$tick_minor_list))
  
  #csu_plot
  if (logscale) {
    base_plot <- ggplot(dt_data[, smooth_value := ifelse(smooth_value==0,NA, smooth_value )], aes(CSU_Y, smooth_value))
  } else {
    base_plot <- ggplot(dt_data, aes(CSU_Y, smooth_value))
  }
  
  csu_plot <- base_plot+
    geom_line(aes(color=CSU_BY), size = 0.75,na.rm=TRUE)+
    guides(color = guide_legend(override.aes = list(size=0.75)))+
    labs(title = plot_title,
         subtitle = NULL,
         caption = NULL)+
    scale_x_continuous(name = "Year",
                       breaks=year_tick$tick_list,
                       limits=c(xlim_inf,xlim_sup),
                       minor_breaks = year_tick$tick_minor_list,
                       expand = c(0.015,0.015)
    )
  
  if (logscale){
    
    
    csu_plot <- csu_plot +
      scale_y_continuous(name = yaxes_title,
                         breaks=tick$tick_list,
                         minor_breaks = tick$tick_minor_list,
                         limits=c(tick$tick_list[1],tick$tick_list[length(tick$tick_list)]),
                         labels=.csu_axes_label,
                         trans = "log10"
      )
  } else {
    
    csu_plot <- csu_plot +
      coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
      scale_y_continuous(name = yaxes_title,
                         breaks=tick$tick_list,
                         labels=.csu_axes_label,
                         expand = c(0,0)
      )
  } 
  
  csu_plot <- csu_plot +
    scale_colour_discrete(name=legend$title)+
    theme(
      plot.background= element_blank(),
      panel.background = element_blank(),
      panel.grid.major= element_line(colour = "grey70"),
      panel.grid.minor= element_line(colour = "grey70"),
      plot.title = element_text(size=16, margin=margin(0,0,15,0),hjust = 0.5),
      plot.subtitle = element_text(size=15, margin=margin(0,0,15,0),hjust = 0.5),
      plot.caption = element_text(size=10, margin=margin(15,0,0,0)),
      axis.title = element_text(size=12),
      axis.title.y = element_text(margin=margin(0,15,0,0)),
      axis.title.x = element_text(margin=margin(15,0,0,0)),
      plot.margin=margin(20,20,20,20),
      axis.text = element_text(size=12, colour = "black"),
      axis.text.x = element_text(size=12,  hjust = 0.5),
      axis.ticks= element_line(colour = "black", size = linesize),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.x = element_line(colour = "black", 
                                 size = linesize, linetype = "solid"),
      axis.line.y = element_line(colour = "black", 
                                 size = linesize, linetype = "solid")
    )+
    th_legend
  

  
  if (!bool_dum_by & legend$position=="right") {
    
    csu_plot <- csu_plot + 
      geom_text(data = dt_data[CSU_Y == max_year, ],
                aes(label = CSU_BY),
                hjust=0,
                nudge_x=0.5)+
      theme(plot.margin = unit(c(0.5, legend$right_space_margin, 0.5, 0.5), "lines"))
    
  } else {
    
    csu_plot <- csu_plot +
      guides(color = guide_legend(nrow=legend$nrow))
  }
  
  gb_plot <- ggplot_build(csu_plot)
  gt_plot <- ggplot_gtable(gb_plot)
  gt_plot$layout$clip[gt_plot$layout$name=="panel"] <- "off"
  if(is.null(format_export)) {
    if (!graph_dev ) {
      
      plot.new()
    }
  }
  grid.draw(gt_plot)
  
  if (!is.null(format_export)) {
    cat("plot exported:\n","\"", getwd(),"/", filename , ".",format_export,"\"",  sep="" )
    dev.off()
  }
  

  
  
  if (bool_dum_by) {
    
    df_data$CSU_dum_by <- NULL
    
  }
  
  if (is.null(smoothing)) {
    dt_data[, smoothing:=NULL]
  }
    
  dt_data[, max_year:=NULL]
  
  setorder(dt_data,CSU_BY,CSU_Y)
  df_data <- data.frame(dt_data)
  setnames(df_data, "CSU_Y", var_year)
  setnames(df_data, "CSU_T", var_trend)
  setnames(df_data,  "CSU_BY", group_by)
  return(invisible(df_data))
  
  
}



  





