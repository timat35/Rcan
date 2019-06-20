csu_bar_top <-
  function(df_data,
   var_value,
   var_bar,
   group_by=NULL,
   nb_top = 10,
   plot_title=NULL,
   plot_subtitle=NULL,
   xtitle= NULL,
   label_by=NULL,
   color=NULL,
   digits = 1) {

  core.error_variable(df_data, var_value, csu_ageSpecific_top)
  core.error_variable(df_data, var_bar, csu_ageSpecific_top, type="")
 
  dt <- data.table(df_data)

  if (!is.null(label_by) & is.null(group_by)) {
    stop(paste0("There is no group_by variable defined (group_by=NULL) to use with the label_by defined: See documentation: Help(", deparse(substitute(csu_bar_top)), ")"))
  }


  csu_ratio = 1
  csu_bar_label_size = 5 
  line_size <- 0.4
  text_size <- 14

  setnames(dt, var_value, "CSU_ASR")
  setnames(dt, var_bar, "CSU_BAR")

  bool_group <- !is.null(group_by)



  if (bool_group) {
     core.error_variable(df_data, group_by, csu_ageSpecific_top, type="")
     setnames(dt, group_by, "CSU_BY")
     if (!is.factor(dt$CSU_BY)) {
      dt[,CSU_BY:=as.factor(CSU_BY)]
    }

    if (length(levels(dt$CSU_BY)) != 2) {
       stop(paste0("group_by variable must have only 2 values: See documentation: Help(", deparse(substitute(csu_bar_top)), ")"))
    }
    if (!is.null(label_by)) {
       dt[,CSU_BY:=factor(CSU_BY, labels=label_by)]
    }

    dt[, CSU_ASR:= as.double(CSU_ASR)] 

    dt_test <- dt[, c("CSU_BAR","CSU_BY"), with=FALSE]
  }
  else {
    dt_test <- dt[, c("CSU_BAR"), with=FALSE]
  }

  
  if (nrow(dt_test) > nrow(unique(dt_test))) {
     stop(paste0("There is more than 1 value for each bar: See documentation: Help(", deparse(substitute(csu_bar_top)), ")"))

  }

  dt <- core.csu_dt_rank(dt, var_value = "CSU_ASR", var_rank = "CSU_BAR",number = nb_top)

  dt_return <- dt
  
  dt$CSU_BAR <-core.csu_legend_wrapper(dt$CSU_BAR, 15)


  if (bool_group) {
    dt[CSU_BY==levels(dt$CSU_BY)[[1]], asr_plot:= CSU_ASR*(-1)]
    dt[CSU_BY==levels(dt$CSU_BY)[[2]], asr_plot:= CSU_ASR]
  }
  else {
    dt[, asr_plot:= CSU_ASR]
  }
  

  dt$CSU_BAR <- factor(dt$CSU_BAR)
  factor_order <- unique(dt[, c("CSU_BAR", "CSU_RANK"), with=FALSE])
  dt$CSU_BAR <- factor(dt$CSU_BAR, levels = rev(setkeyv(factor_order, "CSU_RANK")$CSU_BAR)) 
  


  if (bool_group) {
    tick_minor_list <- core.csu_tick_generator(max = max(dt$CSU_ASR), 0)$tick_list
    nb_tick <- length(tick_minor_list) 
    tick_space <- tick_minor_list[nb_tick] - tick_minor_list[nb_tick-1]
    if ((tick_minor_list[nb_tick] -  max(dt$CSU_ASR))/tick_space < 1/4){
      tick_minor_list[nb_tick+1] <- tick_minor_list[nb_tick] + tick_space
    }
    tick_major <- tick_minor_list[1:length(tick_minor_list) %% 2  == 1]
    tick_major_list <- c(rev(-tick_major),tick_major[tick_major!=0])
    tick_label <- c(rev(tick_major),tick_major[tick_major!=0])
    tick_minor_list <- c(rev(-tick_minor_list),tick_minor_list[tick_minor_list!=0])

  }
  else {
    tick_major_list <- core.csu_tick_generator(max = max(dt$CSU_ASR), 0)$tick_list
    nb_tick <- length(tick_major_list) 
    tick_space <- tick_major_list[nb_tick] - tick_major_list[nb_tick-1]
    if ((tick_major_list[nb_tick] -  max(dt$CSU_ASR))/tick_space < 1/4){
      tick_major_list[nb_tick+1] <- tick_major_list[nb_tick] + tick_space
    }
    
    tick_minor_list <-seq(tick_major_list[1],tail(tick_major_list,1),tick_space/2)
    tick_label <- tick_major_list
  }
  
  dt$asr_label <- dt$CSU_ASR + (tick_space*0.1)
  dt$asr_round <-  format(round(dt$CSU_ASR, digits = digits), nsmall = digits)

  if (bool_group){
    dt[CSU_BY==levels(dt$CSU_BY)[[1]], asr_label:= asr_label*(-1)]
    csu_plot <- ggplot(dt, aes(CSU_BAR, asr_plot, fill=CSU_BY))

     if (is.null(color)) {
      color <- hue_pal()(2)
    }

  }
  else {
     csu_plot <- ggplot(dt, aes(CSU_BAR, asr_plot, fill=CSU_BAR))
     if (is.null(color)) {
      color <- rep(hue_pal()(1), length(levels(dt$CSU_BAR)))
    }
    else if (color[1] %in% names(dt)){

      dt_label_order <- setkey(unique(dt[, c("CSU_ASR",color, "CSU_RANK"), with=FALSE]), CSU_RANK)
      color <- as.character(rev(dt_label_order[[color]]))

    }
    else if (length(color) != length(levels(dt$CSU_BAR))) {
      color <- rep(color[1], length(levels(dt$CSU_BAR)))
    }
  }



  csu_plot <- csu_plot +
    geom_bar(stat="identity", width = 0.8)+
    geom_hline(yintercept = 0, colour="black",size = line_size)


  if (bool_group){
    csu_plot <- csu_plot+
    geom_text(data=dt[asr_label > 0, ],aes(CSU_BAR, asr_label,label=asr_round),
              size = csu_bar_label_size,
              hjust = 0)+
    geom_text(data=dt[asr_label < 0, ],aes(CSU_BAR, asr_label,label=asr_round),
              size = csu_bar_label_size,
              hjust = 1)+
    coord_flip(ylim = c(tick_minor_list[1]-(tick_space*0.25),tick_minor_list[length(tick_minor_list)]+(tick_space*0.25)), expand = TRUE)
  }
  else {
    csu_plot <- csu_plot+
    geom_text(aes(CSU_BAR, asr_label,label=asr_round),
              size = csu_bar_label_size,
              hjust = 0)+
     coord_flip(ylim = c(0,tick_major_list[length(tick_major_list)]+(tick_space*0.25)), expand = TRUE)

  }



  csu_plot <- csu_plot+
  scale_y_continuous(name = xtitle,
                     breaks=tick_major_list,
                     minor_breaks = tick_minor_list,
                     labels=tick_label)+ # tick label
  scale_fill_manual(name="",
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
                               linetype = "solid")
    )

  if (bool_group){
    csu_plot <- csu_plot + 
    theme(
      legend.key = element_rect(fill="transparent"),
      legend.position = "bottom",
      legend.text = element_text(size = text_size),
      legend.key.height = unit(0.6,"cm"),
      legend.key.width =unit(1.5,"cm"),
      legend.margin = margin(0, 0, 0, 0)
      )
  }
  else {
     csu_plot <- csu_plot + 
    theme(legend.position = "none")

  }

  print(csu_plot)

  setnames(dt_return, "CSU_BAR",var_bar)
  setnames(dt_return, "CSU_ASR", var_value)
  setnames(dt_return, "CSU_RANK","cancer_rank")
  setkeyv(dt_return, c("cancer_rank",var_bar))

  if (bool_group) {
    setnames(dt_return, "CSU_BY", group_by)
  }


  return(dt_return)


}