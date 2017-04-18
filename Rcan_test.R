
rcan_folder <- "c:/projects/Rcan"
#install.packages("http://timat.org/matR/Rcan.tar.gz", repos=NULL)
library(Rcan)
library(data.table)
library(ggplot2)
library(grid)

#test package
detach(package:Rcan)
remove.packages("Rcan")
install.packages("C:/Projects/Rcan/Rcan_1.3.0.tar.gz", repos=NULL)

library(Rcan)
data(csu_registry_data_2)
test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]
test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))
df_asr <- csu_asr(test,missing_age = 99,
                  var_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))
csu_trend(df_asr, var_by="sex",
          plot_title = "Colombia, Liver, male", 
          smoothing = 0.4)

help("csu_asr")

source(paste0(rcan_folder, "/helper.r"))


# create function for year axes tick 
# test R package with hidden fonction 


# create csu_time_trend function 
data("csu_registry_data_2")
df_data <- csu_registry_data_2
df_data <- as.data.table(df_data)


levels(df_data$registry_label)[c(5,20,32,45,59,110)]

df_data <- df_data[registry_label %in% levels(df_data$registry_label)[c(5,20,32,45,59,110)],]
df_data <- df_data[sex == 1,]

#df_data$sex <- factor(df_data$sex, levels=c(1,2), labels=c("Male", "Female"))
df_asr <- csu_asr(df_data = df_data,missing_age = 99,
                  var_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))


df_data <- as.data.table(df_asr)


#function argument 

var_trend = "asr"
var_year = "year"
var_by = "registry_label"
logscale = TRUE
smoothing = 0.3
legend = csu_trend_legend()
plot_title = "csu_title"
yaxes_title = "Age standardized (world) rate per 100,000"
format_export = "tiff"

bool_dum_by <- FALSE

if (is.null(var_by)) {
  
  df_data$CSU_dum_by <- "dummy_by"
  var_by <- "CSU_dum_by"
  bool_dum_by <- TRUE
}

dt_data <- data.table(df_data, key = var_by)

setnames(dt_data, var_year, "CSU_Y")
setnames(dt_data, var_trend, "CSU_T")
setnames(dt_data, var_by, "CSU_BY")

#change by to factor
dt_data$CSU_BY <- factor(dt_data$CSU_BY)



if (!is.null(smoothing))
{
  dt_data[,CSU_T:= loess( CSU_T ~ CSU_Y, span=smoothing)$fitted, by=CSU_BY]
}



# to calcul breaks
tick <- csu_tick_generator(max = max(dt_data$CSU_T), min=min(dt_data[CSU_T != 0,]$CSU_T), log_scale = logscale )
tick_space <- tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list)-1]


year_tick <- csu_year_tick_generator(min(dt_data$CSU_Y),max(dt_data$CSU_Y))


temp_top <- ceiling(max(dt_data$CSU_T)/tick_space)*tick_space
temp_expand_y <- max(dt_data$CSU_T)/35
temp_expand_y_up <- max(dt_data$CSU_T)+temp_expand_y
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
csu_format_export(format_export, plot_title = plot_title)

#csu_plot
if (logscale) {
  base_plot <- ggplot(dt_data[, CSU_T := ifelse(CSU_T==0,NA, CSU_T )], aes(CSU_Y, CSU_T))
} else {
  base_plot <- ggplot(dt_data, aes(CSU_Y, CSU_T))
}

csu_plot <- base_plot+
  geom_line(aes(color=CSU_BY), size = 0.75,na.rm=TRUE)+
  guides(color = guide_legend(override.aes = list(size=0.75)))+
  labs(title = plot_title,
       subtitle = NULL,
       caption = NULL)+
  scale_x_continuous(name = "Year",
                     breaks=year_tick$tick_list,
                     limits=c(year_tick$tick_list[1],year_tick$tick_list[length(year_tick$tick_list)]),
                     minor_breaks = year_tick$tick_minor_list,
                     expand = c(0.015,0.015)
  )

if (logscale){
  
  
  csu_plot <- csu_plot +
    scale_y_continuous(name = yaxes_title,
                       breaks=tick$tick_list,
                       minor_breaks = tick$tick_minor_list,
                       limits=c(tick$tick_list[1],tick$tick_list[length(tick$tick_list)]),
                       labels=csu_axes_label,
                       trans = "log10"
    )
} else {
  
  csu_plot <- csu_plot +
    coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
    scale_y_continuous(name = paste("ASR", formatC(100000, format="d", big.mark=",")),
                       breaks=tick$tick_list,
                       labels=csu_axes_label,
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
    axis.ticks= element_line(colour = "black", size = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x = element_line(colour = "black", 
                               size = 0.5, linetype = "solid"),
    axis.line.y = element_line(colour = "black", 
                               size = 0.5, linetype = "solid")
  )+
  th_legend



if (!bool_dum_by & legend$position=="right") {
  
  csu_plot <- csu_plot + 
    geom_text(data = dt_data[CSU_Y == max_year, ],
              aes(label = CSU_BY),
              hjust=-0.25)+
    theme(plot.margin = unit(c(0.5, legend$right_space_margin, 0.5, 0.5), "lines"))
  
} else {
  
  csu_plot <- csu_plot +
    guides(color = guide_legend(nrow=legend$nrow))
}

gb_plot <- ggplot_build(csu_plot)
gt_plot <- ggplot_gtable(gb_plot)
gt_plot$layout$clip[gt_plot$layout$name=="panel"] <- "off"
if(is.null(format_export)) {
  plot.new()
}
grid.draw(gt_plot)

if (!is.null(format_export)) {
  dev.off()
}






