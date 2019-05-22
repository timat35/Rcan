csu_bar_top <-
  function(df_data,
   var_top,
   var_bar,
   group_by,
   nb_top = 10,
   plot_title=NULL,
   plot_subtitle=NULL,
   ytitle= NULL,
   color=c("#2c7bb6","#b62ca1"),
   digits = 1) {

  Rcan:::core.error_variable(df_data, var_top, csu_ageSpecific_top)
  Rcan:::core.error_variable(df_data, var_bar, csu_ageSpecific_top, type="")
  Rcan:::core.error_variable(df_data, group_by, csu_ageSpecific_top, type="")


  #add test for groupby number of levels

  
  dt <- data.table(df_data)
  
  #if (landscape) {
  #  csu_ratio = 0.6 
  #  csu_bar_label_size = 4
  #} else {
    csu_ratio = 1
    csu_bar_label_size = 5 
  #}
  
  line_size <- 0.4
  text_size <- 14

  
  setnames(dt, var_top, "CSU_ASR")
  setnames(dt, var_bar, "CSU_BAR")
  setnames(dt, group_by, "CSU_BY")
  
  if (is.null(plot_subtitle)){
    plot_subtitle <- paste0("top"," ",nb_top," ",var_bar)
  }
  
  dt <- Rcan:::core.csu_dt_rank(dt, var_value = "CSU_ASR", var_rank = "CSU_BAR",number = nb_top)
  
  dt$CSU_BAR <-Rcan:::core.csu_legend_wrapper(dt$CSU_BAR, 15)
  dt[CSU_BY==levels(dt$CSU_BY)[[1]], asr_plot:= CSU_ASR*(-1)]
  dt[CSU_BY==levels(dt$CSU_BY)[[2]], asr_plot:= CSU_ASR]
  
  dt$CSU_BAR <- factor(dt$CSU_BAR)
  factor_order <- unique(dt[, c("CSU_BAR", "CSU_RANK"), with=FALSE])
  dt$CSU_BAR <- factor(dt$CSU_BAR,
                       levels = rev(setkeyv(factor_order, "CSU_RANK")$CSU_BAR)) 
  
  tick_minor_list <- Rcan:::core.csu_tick_generator(max = max(dt$CSU_ASR), 0)$tick_list
  nb_tick <- length(tick_minor_list) 
  tick_space <- tick_minor_list[nb_tick] - tick_minor_list[nb_tick-1]
  if ((tick_minor_list[nb_tick] -  max(dt$CSU_ASR))/tick_space < 1/4){
    tick_minor_list[nb_tick+1] <- tick_minor_list[nb_tick] + tick_space
  }
  tick_major <- tick_minor_list[1:length(tick_minor_list) %% 2  == 1]
  tick_major_list <- c(rev(-tick_major),tick_major[tick_major!=0])
  tick_label <- c(rev(tick_major),tick_major[tick_major!=0])
  tick_minor_list <- c(rev(-tick_minor_list),tick_minor_list[tick_minor_list!=0])
  
  dt$asr_label <- dt$CSU_ASR + (tick_space*0.1)

  dt[CSU_BY==levels(dt$CSU_BY)[[1]], asr_label:= asr_label*(-1)]
  dt$asr_round <-  format(round(dt$CSU_ASR, digits = 1), nsmall = digits)
  
  csu_plot <- ggplot(dt, aes(CSU_BAR, asr_plot, fill=CSU_BY)) +
    geom_bar(stat="identity", width = 0.8)+
    geom_hline(yintercept = 0, colour="black",size = line_size)+
    geom_text(data=dt[asr_label > 0, ],aes(CSU_BAR, asr_label,label=asr_round),
              size = csu_bar_label_size,
              hjust = 0)+
    geom_text(data=dt[asr_label < 0, ],aes(CSU_BAR, asr_label,label=asr_round),
              size = csu_bar_label_size,
              hjust = 1)+
    coord_flip(ylim = c(tick_minor_list[1]-(tick_space*0.25),tick_minor_list[length(tick_minor_list)]+(tick_space*0.25)), expand = TRUE)+
    scale_y_continuous(name = ytitle,
                       breaks=tick_major_list,
                       minor_breaks = tick_minor_list,
                       labels=tick_label
    )+
    scale_fill_manual(name="",
                      #labels = c(i18n$t("Male"),i18n$t("Female")),
                      values= color,
                      drop = FALSE)+
    labs(title = plot_title, 
         subtitle = plot_subtitle)+
    theme(
      aspect.ratio = csu_ratio,
      plot.background= element_blank(),
      panel.background = element_blank(),
      panel.grid.major.y= element_blank(),
      panel.grid.major.x= element_line(colour = "grey70",size = line_size),
      panel.grid.minor.x= element_line(colour = "grey70",size = line_size),
      plot.title = element_text(size=18, margin=margin(0,0,15,0),hjust = 0.5),
      plot.subtitle = element_text(size=16, margin=margin(0,0,15,0),hjust = 0.5),
      plot.caption = element_text(size=12, margin=margin(15,0,0,0)),
      plot.margin=margin(20,20,20,20),
      axis.title = element_text(size=text_size),
      axis.title.x=element_text(margin=margin(10,0,0,0)),
      axis.title.y = element_blank(),
      axis.text = element_text(size=text_size, colour = "black"),
      axis.text.x = element_text(size=text_size),
      axis.text.y = element_text(size=text_size),
      axis.ticks.x= element_line(colour = "black", size = line_size),
      axis.ticks.y= element_blank(),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(colour = "black", 
                                 size = line_size, 
                                 linetype = "solid"),
      legend.key = element_rect(fill="transparent"),
      legend.position = "bottom",
      legend.text = element_text(size = text_size),
      legend.key.height = unit(0.6,"cm"),
      legend.key.width =unit(1.5,"cm"),
      legend.margin = margin(0, 0, 0, 0)
    )


  print(csu_plot)


  dt[, rank_value := NULL]
  setnames(dt, "CSU_BAR",var_bar)
  setnames(dt, "CSU_BY", group_by)
  setnames(dt, "CSU_ASR", var_top)
  setnames(dt, "CSU_RANK","cancer_rank")
  setkeyv(dt, c("cancer_rank",var_bar))
  return(dt)

}