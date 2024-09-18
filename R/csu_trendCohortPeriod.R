csu_trendCohortPeriod <- function (
  df_data,
  var_age = "age",
  var_cases="cases",
  var_py="py",
  var_year = "year",
  type = "Cohort",
  missing_age = NULL,
  logscale = TRUE,
  db_rate = 100000,
  first_age = 6,
  last_age = 16,
  year_group = 5,
  age_dropped=FALSE,
  plot_title = "csu_title",
  format_export = NULL,
  graph_dev =FALSE) {
  
  linesize <- 0.75
  landscape <- FALSE

  if (!is.null(missing_age)) {
    core.error_missingage(df_data, var_age,missing_age, csu_trendCohortPeriod)
  }

  core.error_age_parse(df_data, var_age, missing_age, csu_trendCohortPeriod)

  dt_data <- data.table(df_data)
  setnames(dt_data, var_age, "CSU_A")
  setnames(dt_data, var_cases, "CSU_C")
  setnames(dt_data, var_py, "CSU_P")
  setnames(dt_data, var_year, "CSU_Y")
  
  #group population (use sum)
  dt_data <- dt_data[, list(CSU_C=sum(CSU_C),CSU_P=sum(CSU_P)), by=c("CSU_A", "CSU_Y") ]
  
  #generate grouped year period
  
  if (year_group > 1) {
    
    max_year <- max(dt_data$CSU_Y)
    min_year <- min(dt_data$CSU_Y)
    temp <- min_year+((max_year - min_year+1)%%year_group)
    dt_data<- dt_data[CSU_Y >= temp,]
    dt_data[, CSU_Y:=cut(CSU_Y,seq(temp,max_year+1,year_group), right = FALSE, labels = seq(temp+((year_group-1)/2),max_year+1,year_group ))]
    dt_data[, CSU_Y:=as.numeric(levels(CSU_Y))[CSU_Y]]
    
  }
  
  
  
  
  #group population (use sum)
  dt_data <- dt_data[, list(CSU_C=sum(CSU_C),CSU_P=sum(CSU_P)), by=c("CSU_A", "CSU_Y") ]
  
  #drop missing age
  if (!is.null(missing_age)) {
    dt_data <- dt_data[dt_data$CSU_A!=missing_age] 
  }
  

  #parse age
    dt_data[,CSU_A :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",CSU_A, perl=TRUE))]
    if (max(dt_data$CSU_A,na.rm=TRUE) > 25) {
      dt_data[,CSU_A := round((CSU_A/5)+1)]
  }

  #create age dummy: 1 2 3 4 --- 18
  dt_data$CSU_age_factor <- as.numeric(c(as.factor(dt_data$CSU_A)))
  
  #age dropped option
  if (age_dropped) {
    dt_data$CSU_age_factor <- dt_data$CSU_age_factor + first_age -1   
  }
  
  # keep age selected 
  dt_data=dt_data[dt_data$CSU_age_factor %in% c(first_age:last_age) | is.na(dt_data$CSU_age_factor), ]
  
  if (last_age == 18) {
    dt_data[dt_data$CSU_P != 0,nb_age_group := max(CSU_age_factor)] 
  } else {
    dt_data$nb_age_group <- 18
  }
  
  #regroup case for population with nb of age group <  18 
  for (i in 15:17) {
    if (i %in% dt_data$nb_age_group) {
      dt_data[dt_data$nb_age_group == i & dt_data$CSU_age_factor >= i , CSU_C:=sum(CSU_C), by="CSU_BY"] #add total_know
      dt_data[dt_data$nb_age_group == i & dt_data$CSU_age_factor > i & !is.na(dt_data$CSU_age_factor), CSU_C := 0] 
    } 
  }
  
  
  #create age label
  dt_data[, temp_age:=(CSU_age_factor-1)*5]
  
  
  
  if (last_age == 18) {
    dt_data[temp_age < max(temp_age) ,CSU_age_label:=paste0(temp_age, "-", temp_age+4)]
    dt_data[temp_age == max(temp_age),CSU_age_label:=paste0(temp_age, "+")]
  } else {
    dt_data[,CSU_age_label:=paste0(temp_age, "-", temp_age+4)]
  }
  
  #create cohort year 
  dt_data[,CSU_cohort:=CSU_Y - temp_age - 2.5]
  
  #calcul rate 
  dt_data$CSU_rate <- dt_data$CSU_C/dt_data$CSU_P *db_rate
  
  # to calcul y axes breaks
  tick <- core.csu_tick_generator(max = max(dt_data$CSU_rate), min=min(dt_data[CSU_rate != 0,]$CSU_rate), logscale = logscale )
  tick_space <- tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list)-1]
  
  
  #to calcul year axes break
  if (type == "Period") {
    year_tick <- core.csu_year_tick_generator(min(dt_data$CSU_Y),max(dt_data$CSU_Y))
  } else if (type == "Both"){
    landscape <- TRUE
    year_tick <- core.csu_year_tick_generator(min(dt_data$CSU_cohort),max(dt_data$CSU_Y))
  } else {
    year_tick <- core.csu_year_tick_generator(min(dt_data$CSU_cohort),max(dt_data$CSU_cohort))
  }
  
  
  temp_top <- ceiling(max(dt_data$CSU_rate)/tick_space)*tick_space
  temp_expand_y <- max(dt_data$CSU_rate)/35
  temp_expand_y_up <- max(dt_data$CSU_rate)+temp_expand_y
  if (temp_expand_y_up > temp_top-(tick_space/2)) {
    temp_expand_y_up <- temp_top+temp_expand_y
  }
  
  th_legend <- list(theme(legend.position="none"))
  
  dt_data$CSU_age_factor <- as.factor(dt_data$CSU_age_factor)
  
  #get color gradient
  color_scale <- scales::seq_gradient_pal(low = "#60b1e2", high = "#0b3e6b")(seq(0,1,length.out=length(levels(dt_data$CSU_age_factor))))
  
  
  
  #format
  if (!is.null(format_export)) {
    filename <- gsub("[[:punct:][:space:]\n]", "_", plot_title)
    core.csu_format_export(format_export, plot_title = filename, landscape = landscape)
  }
  
  
  if (type == "Period") {
    dt_data[, CSU_Xaxes:=CSU_Y]
    Xaxes_label <- "Year of diagnosis"
  } else if (type == "Both") {
    dt_data[, CSU_Xaxes:=CSU_cohort]
    Xaxes_label <- "Year of birth   |   Year of diagnosis"
  } else  {
    dt_data[, CSU_Xaxes:=CSU_cohort]
    Xaxes_label <- "Year of birth"

  }
  
  dt_data[, temp_label:=max(CSU_Xaxes), by=CSU_age_factor]
  
  
  if (type == "both") {
    dt_data[, temp_label2:=max(CSU_Y), by=CSU_age_factor]
    dt_data[CSU_Xaxes != temp_label & CSU_Y !=temp_label2 , CSU_age_label:=NA]
    
  } else {
    
    dt_data[CSU_Xaxes!=temp_label, CSU_age_label:=NA]
    
  }
  
  xlim_inf <- min(c(year_tick$tick_list, year_tick$tick_minor_list))
  xlim_sup <- max(c(year_tick$tick_list, year_tick$tick_minor_list))
  
  #csu_plot
  if (logscale) {
    base_plot <- ggplot(dt_data[, CSU_rate := ifelse(CSU_rate==0,NA, CSU_rate )], aes(y=CSU_rate))
  } else {
    base_plot <- ggplot(dt_data, aes(CSU_Xaxes, CSU_rate))
  }
  
  csu_plot <- base_plot +
    geom_line(aes(x=CSU_Xaxes, color=CSU_age_factor), size = 1,na.rm=TRUE)+
    geom_text(data = dt_data[!is.na(CSU_age_label)],
              aes(x=CSU_Xaxes, label = CSU_age_label),
              size=2.5,
              check_overlap = T,
              nudge_x=0.5,
              hjust=0)
  
  if (type == "Both") {
    csu_plot <- csu_plot+
      geom_line(aes(x=CSU_Y, color=CSU_age_factor), size = 1,na.rm=TRUE)+
      geom_text(data = dt_data[!is.na(CSU_age_label)],
                aes(x=CSU_Y, label = CSU_age_label),
                size=2.5,
                check_overlap = T,
                nudge_x=0.5,
                hjust=0)
    
  } 
  
  
  csu_plot <- csu_plot+
    labs(title = plot_title,
         subtitle = NULL,
         caption = NULL)+
    scale_x_continuous(name = Xaxes_label,
                       breaks=year_tick$tick_list,
                       limits=c(xlim_inf,xlim_sup),
                       minor_breaks = year_tick$tick_minor_list,
                       expand = c(0.015,0.015)
    )
  
  if (type == "Both") {
    
    diff <- min(dt_data$CSU_Y) - max(dt_data$CSU_cohort)
    if (diff > 0) {
      
      csu_plot <- csu_plot +
        geom_vline(xintercept = min(dt_data$CSU_Y)-(diff/2), colour="black",size = 0.5)
      
    }
    
  }
  
  csu_plot <- csu_plot +
    geom_point(aes(x=CSU_Xaxes),fill="black", size = 1.5,na.rm=TRUE,shape=21,stroke=1,colour="black", show.legend=FALSE)
  
  if (type == "Both") {
    csu_plot <- csu_plot+
      geom_point(aes(x=CSU_Y),fill="black", size = 1.5,na.rm=TRUE,shape=21,stroke=1,colour="black", show.legend=FALSE)
    
    
  }
  
  
  if (logscale){
    

    
    csu_plot <- csu_plot +
      scale_y_continuous(name = paste("Age-specific rate per", formatC(db_rate, format="d")),
                         breaks=tick$tick_list,
                         minor_breaks = tick$tick_minor_list,
                         limits=c(tick$tick_list[1],tick$tick_list[length(tick$tick_list)]),
                         labels=core.csu_axes_label,
                         trans = "log10"
      )
  } else {
    
    csu_plot <- csu_plot +
      coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
      scale_y_continuous(name = paste("Age-specific rate per", formatC(db_rate, format="d")),
                         breaks=tick$tick_list,
                         labels=core.csu_axes_label,
                         expand = c(0,0)
      )
  }
  
  
  
  
  csu_plot <- csu_plot +
    scale_colour_manual(values=color_scale)+
    theme(
      plot.background= element_blank(),
      panel.background = element_blank(),
      panel.grid.major= element_line(colour = "grey70"),
      panel.grid.minor= element_line(colour = "grey70"),
      plot.title = element_text(size=16, margin=margin(0,0,15,0),hjust = 0.5),
      plot.subtitle = element_text(size=15, margin=margin(0,0,15,0),hjust = 0.5),
      plot.caption = element_text(size=10, margin=margin(15,0,0,0)),
      axis.title = element_text(size=12),
      axis.title.y = element_text(margin=margin(0,5,0,0)),
      axis.title.x = element_text(margin=margin(15,0,0,0)),
      plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines"),
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
    cat("plot exported:\n","\"", getwd(),"/", filename , ".",format_export,"\"\n",  sep="" )
    dev.off()
  }
  


  
  dt_data[, CSU_age_factor:=NULL]
  dt_data[, nb_age_group:=NULL]
  dt_data[, temp_age:=NULL]
  dt_data[, CSU_age_label:=NULL]
  dt_data[, CSU_Xaxes:=NULL]
  dt_data[, temp_label:=NULL]

  
  #setorder(dt_data,CSU_BY,CSU_Y)
  df_data <- data.frame(dt_data)
  
  setnames(df_data, "CSU_A", var_age)
  setnames(df_data, "CSU_C", var_cases)
  setnames(df_data, "CSU_P", var_py)
  setnames(df_data, "CSU_Y", var_year)

  return(invisible(df_data))
  
  
  
}



  





