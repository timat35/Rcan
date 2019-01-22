# source file of functions for oesophageal trends project

library(tidyverse)
library(Rcan)
library(data.table)
library(ggplot2)


# plot proportions of oesophageal cancer cases over time
prop_graph <- function(df, int_sex, int_group) {
  plot <-
    df %>%
    filter(sex == int_sex & group == int_group) %>%
    filter(cancer != 11 &
             cancer != 14 &
             cancer != 15 &
             cancer != 16 &
             cancer != 16 & cancer != 17 & cancer != 18) %>%
    ggplot(aes(x = year, y = prop, group = histo_lab)) +
    geom_line(aes(color = histo_lab)) +
    xlim(1960, 2015) +
    facet_wrap(~ registry_label)
  
  return(plot)
  
  
}

# function to produce EAPC forest plot by sex and cancer histology
rcan_scatter_error_bar <- function(dt,
                                   var_data = "eapc",
                                   var_data_up = "eapc_up",
                                   var_data_low = "eapc_low",
                                   var_bar = "registry_label",
                                   ytitle = "Estimated annual percentage change (%)",
                                   int_sex,
                                   int_cancer) {
  dt_plot <- dt[sex == int_sex & cancer == int_cancer, ]
  
  line_size <- 0.4
  text_size <- 11
  
  #calculate tick list (using Rcan hidden function)
  tick <-
    Rcan:::core.csu_tick_generator(max = max(dt_plot$eapc_up),
                                   min = min(dt_plot$eapc_low))
  
  #to have positive and negative side
  tick_space <-
    tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list) -
                                                              1]
  if (min(tick$tick_list) == 0) {
    tick$tick_list <- c(-tick_space, tick$tick_list)
  }
  if (max(tick$tick_list) == 0) {
    tick$tick_list <- c(tick$tick_list, tick_space)
  }
  
  
  dt_label_order <-
    setkeyv(unique(dt_plot[, c("registry_label", "eapc"), with = FALSE]), c("eapc"))
  dt_plot$registry_label <-
    factor(dt_plot$registry_label,
           levels = unique(dt_label_order$registry_label, fromLast = TRUE))
  
  csu_plot <-
    ggplot(dt_plot, aes(eapc, registry_label)) +
    geom_errorbarh(
      aes(xmin = eapc_low , xmax = eapc_up),
      size = 0.7,
      height = 0.5,
      colour = "#05305b"
    ) +
    geom_point(
      fill = "#e41a1c",
      shape = 21,
      color = "black",
      size = 3,
      stroke = 1.2
    )
  
  
  csu_plot <- csu_plot +
    scale_x_continuous(
      name = ytitle,
      breaks = tick$tick_list,
      limits = c(tick$tick_list[1], tick$tick_list[length(tick$tick_list)]),
      labels = Rcan:::core.csu_axes_label
    )
  
  
  csu_plot <- csu_plot +
    geom_vline(xintercept = 0, size = 0.8) +
    labs(title = NULL,
         subtitle = NULL,
         caption = NULL) +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.y =  element_blank(),
      panel.grid.major.x = element_line(colour = "grey70", size = line_size),
      panel.grid.minor.x = element_line(colour = "grey70", size = line_size),
      plot.title = element_text(
        size = 18,
        margin = margin(0, 0, 15, 0),
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 16,
        margin = margin(0, 0, 15, 0),
        hjust = 0.5
      ),
      plot.caption = element_text(size = 12, margin = margin(15, 0, 0, 0)),
      plot.margin = margin(20, 20, 20, 20),
      axis.title = element_text(size = text_size),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
      axis.title.y = element_blank(),
      axis.text = element_text(size = text_size, colour = "black"),
      axis.text.x = element_text(size = text_size),
      axis.text.y = element_text(size = text_size),
      axis.ticks.x = element_line(colour = "black", size = line_size),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(
        colour = "black",
        size = line_size,
        linetype = "solid"
      ),
      legend.key = element_rect(fill = "transparent"),
      legend.position = "bottom",
      legend.text = element_text(size = text_size),
      legend.key.height = unit(0.6, "cm"),
      legend.key.width = unit(1.5, "cm"),
      legend.margin = margin(0, 0, 0, 0)
    )
  
  print(csu_plot)
  
}



# USEFUL CODE
# o3 %>%
#   filter(!registry_label %in% c("Australia_2_all", "China_2_all")) -> o3

# get("o4")
# data_list <- list[age1,age2,age3]

# for (sex in 1:2)
#  for (group in 1:4) {
#    for (data in data_list) {
#
#      prop_graph(get(data),sex,group)
#
#      if (sex == 1) {
#        sex_label = "m"
#      }
#      else {
#        sex_label = "f"
#      }
#
#      file_label <- paste0("oes_prop_trend_",sex_label,group)
#      file_pdf <- paste0(file_label, ".pdf")
#      ggsave(file_pdf)
#      ggsave(file_png)
#
#    }
#  }

HR_core.csu_time_trend <- function (df_data,
                                    var_trend = "asr",
                                    var_year = "year",
                                    xlims = NULL,
                                    ylims = NULL,
                                    group_by = NULL,
                                    logscale = TRUE,
                                    smoothing = NULL,
                                    legend = csu_trend_legend(),
                                    color_trend = NULL,
                                    ytitle = "Age standardized rate per 100,000",
                                    xtitle = "Year",
                                    plot_title = "test",
                                    linesize = 0.5,
                                    plot_subtitle = NULL,
                                    plot_caption = NULL) {
  if (!is.null(smoothing)) {
    if (smoothing == 0) {
      smoothing <- NULL
    }
  }
  
  
  
  dt_data <- data.table(df_data, key = group_by)
  setnames(dt_data, var_year, "CSU_Y")
  setnames(dt_data, var_trend, "CSU_T")
  setnames(dt_data, group_by, "CSU_BY")
  
  bool_dum_by <- FALSE
  if (is.null(group_by)) {
    dt_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
  }
  
  #change by to factor
  dt_data$CSU_BY <- factor(dt_data$CSU_BY)
  
  #smooth with loess  fonction
  if (!is.null(smoothing))
  {
    dt_data[, CSU_smooth := loess(CSU_T ~ CSU_Y, span = smoothing)$fitted, by =
              CSU_BY]
  } else {
    dt_data[, CSU_smooth := CSU_T]
  }
  
  dt_data[, max_year := max(CSU_Y), by = CSU_BY]
  
  # to calcul y axes breaks
  
  if (!is.null(ylims) & logscale) {
    min_tick_value <- ylims[1]
  } else if (!is.null(ylims) & is.null(logscale)) {
    min_tick_value <- ylims[1]
  } else if (logscale & is.null(ylims)) {
    min_tick_value <- min(dt_data[CSU_smooth > 0, ]$CSU_smooth)
  } else {
    min_tick_value <- 0
  }
  
  if (is.null(ylims)) {
    tick <-
      Rcan:::core.csu_tick_generator(
        max = max(dt_data$CSU_smooth),
        min = min_tick_value,
        logscale = logscale
      )
  } else {
    tick <-
      Rcan:::core.csu_tick_generator(max = ylims[2],
                                     min = min_tick_value,
                                     logscale = logscale)
  }
  
  tick_space <-
    tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list) -
                                                              1]
  
  
  #to calcul year axes break
  
  if (is.null(xlims)) {
    year_tick <-
      Rcan:::core.csu_year_tick_generator(min(dt_data$CSU_Y), max(dt_data$CSU_Y))
  } else {
    year_tick <-  Rcan:::core.csu_year_tick_generator(xlims[1], xlims[2])
  }
  
  if (is.null(ylims)) {
    temp_top <- ceiling(max(dt_data$CSU_smooth) / tick_space) * tick_space
    temp_expand_y <- max(dt_data$CSU_smooth) / 35
    temp_expand_y_up <- max(dt_data$CSU_smooth) + temp_expand_y
  } else {
    temp_top <- ceiling(ylims[2] / tick_space) * tick_space
    temp_expand_y <- ylims[2] / 35
    temp_expand_y_up <- ylims[2] + temp_expand_y
  }
  
  if (temp_expand_y_up > temp_top - (tick_space / 2)) {
    temp_expand_y_up <- temp_top + temp_expand_y
  }
  
  th_legend <- list(theme(legend.position = "none"))
  
  if (!bool_dum_by & legend$position == "bottom") {
    th_legend <- list(
      theme(
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        legend.margin = margin(0, 0, 0, 0)
      )
    )
  }
  
  
  xlim_inf <- min(c(year_tick$tick_list, year_tick$tick_minor_list))
  xlim_sup <- max(c(year_tick$tick_list, year_tick$tick_minor_list))
  
  ylim_inf <- min(c(tick$tick_list, tick$tick_minor_list))
  ylim_sup <- max(c(tick$tick_list, tick$tick_minor_list))
  
  
  #csu_plot
  if (logscale) {
    base_plot <-
      ggplot(dt_data[, CSU_smooth := ifelse(CSU_smooth == 0, NA, CSU_smooth)], aes(CSU_Y, CSU_smooth))
  } else {
    base_plot <- ggplot(dt_data, aes(CSU_Y, CSU_smooth))
  }
  
  csu_plot <- base_plot +
    geom_line(aes(color = CSU_BY), size = 0.75, na.rm = TRUE) +
    guides(color = guide_legend(override.aes = list(size = 0.75))) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         caption = plot_caption) +
    scale_x_continuous(
      name = xtitle,
      breaks = year_tick$tick_list,
      limits = c(xlim_inf, xlim_sup),
      minor_breaks = year_tick$tick_minor_list,
      expand = c(0.015, 0.015)
    )
  
  
  
  if (logscale) {
    csu_plot <- csu_plot +
      scale_y_continuous(
        name = ytitle,
        breaks = tick$tick_list,
        minor_breaks = tick$tick_minor_list,
        limits = c(ylim_inf, ylim_sup),
        labels = Rcan:::core.csu_axes_label,
        trans = "log10"
      )
  } else {
    csu_plot <- csu_plot +
      coord_cartesian(ylim = c(-temp_expand_y, temp_expand_y_up),
                      expand = TRUE) +
      scale_y_continuous(
        name = ytitle,
        breaks = tick$tick_list,
        labels = Rcan:::core.csu_axes_label,
        expand = c(0, 0)
      )
  }
  
  if (is.null(color_trend)) {
    csu_plot <- csu_plot + scale_colour_discrete(name = legend$title)
  }
  else {
    csu_plot <-
      csu_plot + scale_colour_manual(name = NULL, values = color_trend)
  }
  
  
  csu_plot <- csu_plot +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey70"),
      panel.grid.minor = element_line(colour = "grey70"),
      plot.title = element_text(
        size = 16,
        margin = margin(0, 0, 15, 0),
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 15,
        margin = margin(0, 0, 15, 0),
        hjust = 0.5
      ),
      plot.caption = element_text(size = 10, margin = margin(15, 0, 0, 0)),
      axis.title = element_text(size = 12),
      axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
      axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
      plot.margin = margin(20, 20, 20, 20),
      axis.text = element_text(size = 12, colour = "black"),
      axis.text.x = element_text(size = 12,  hjust = 0.5),
      axis.ticks = element_line(colour = "black", size = linesize),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.x = element_line(
        colour = "black",
        size = linesize,
        linetype = "solid"
      ),
      axis.line.y = element_line(
        colour = "black",
        size = linesize,
        linetype = "solid"
      )
    ) +
    th_legend
  
  
  
  if (!bool_dum_by & legend$position == "right") {
    csu_plot <- csu_plot +
      geom_text(
        data = dt_data[CSU_Y == max_year,],
        aes(label = CSU_BY),
        hjust = 0,
        nudge_x = 0.5
      ) +
      theme(plot.margin = unit(c(
        0.5, legend$right_space_margin, 0.5, 0.5
      ), "lines"))
    
  } else {
    csu_plot <- csu_plot +
      guides(color = guide_legend(nrow = legend$nrow))
  }
  
  return(
    list(
      dt_data = dt_data,
      csu_plot = csu_plot,
      legend_position = legend$position,
      bool_dum_by = bool_dum_by
    )
  )
  
  
}


HR_csu_time_trend <- function (df_data,
                               var_trend = "asr",
                               var_year = "year",
                               xlims = NULL,
                               ylims = NULL,
                               group_by = NULL,
                               logscale = FALSE,
                               smoothing = NULL,
                               legend = csu_trend_legend(),
                               color_trend = NULL,
                               ytitle = "Age standardized rate per 100,000",
                               plot_title = "csu_title",
                               int_sex = "sex",
                               int_regist = "registry_label") {
  linesize <- 0.75
  
  Rcan:::core.error_variable(df_data, var_trend, csu_time_trend)
  Rcan:::core.error_variable(df_data, var_year, csu_time_trend)
  Rcan:::core.error_time_variable(df_data, var_year, group_by, csu_time_trend)
  
  
  #check by variable adapted (ie: 1 year per variable)
  dt_data <- data.table(df_data, key = group_by)
  if (is.null(group_by)) {
    dt_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
  }
  
  dt_data$temp <- 1
  nrow_base <- nrow(dt_data)
  dt_test <- dt_data[, temp := sum(temp), by = c(var_year,  group_by)]
  nrow_test <-
    nrow(dt_data[, sum(temp), by = c(var_year,  group_by)])
  dt_data$temp <- NULL
  
  if (nrow_test != nrow_base) {
    setkeyv(dt_test, c(group_by, var_year))
    print(head(dt_test[temp > 1,]))
    dt_data <- NULL
    stop(
      "There is more than 1 data per year (see above).\nUse the 'group_by' option or call the function on a subset to define the sub-population of interest.\n"
    )
  }
  
  
  #call to core function
  csu_list <- HR_core.csu_time_trend(
    df_data,
    var_trend,
    var_year,
    xlims,
    ylims,
    group_by,
    logscale,
    smoothing,
    legend,
    color_trend,
    ytitle,
    plot_title = plot_title,
    linesize = linesize
  )
  
  dt_data <- csu_list$dt_data
  
  if (csu_list$legend_position == "right") {
    gb_plot <- ggplot_build(csu_list$csu_plot)
    gt_plot <- ggplot_gtable(gb_plot)
    gt_plot$layout$clip[gt_plot$layout$name == "panel"] <- "off"
    grid.draw(gt_plot)
    
  } else {
    print(csu_list$csu_plot)
  }
  
}



