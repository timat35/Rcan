
rcan_folder <- "c:/projects/Rcan"
#install.packages("http://timat.org/matR/Rcan.tar.gz", repos=NULL)
library(Rcan)
library(data.table)
library(ggplot2)
library(grid)

source(paste0(rcan_folder, "/csu_trend.r"))
source(paste0(rcan_folder, "/helper.r"))



#test package
#detach(package:Rcan)
#remove.packages("Rcan")
#install.packages("C:/Projects/Rcan/Rcan_1.3.0.tar.gz", repos=NULL)

library(Rcan)
data(csu_registry_data_2)
test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]
test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))

df_asr <- csu_asr(test,missing_age = 99,
                  var_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))

csu_trend(df_asr, var_by="sex",
          plot_title = "Colombia, Liver, male", 
          smoothing = 0.4,
          format_export = "pdf")
getwd()
help("csu_asr")

source(paste0(rcan_folder, "/helper.r"))


# create csu_period_cohort function 
data(csu_registry_data_2)
df_data <- csu_registry_data_2[csu_registry_data_2$registry_label=="USA, SEER (9 registries)",]
df_data <- df_data[df_data$sex==1,]


#function argument 

var_age = "age"
var_cases="cases"
var_py="py"
var_year="year"
missing_age = 99
db_rate = 100000
logscale = TRUE
plot_title = "CSU_title"
format_export = NULL
first_age = 5
last_age = 16


dt_data <- data.table(df_data)
setnames(dt_data, var_age, "CSU_A")
setnames(dt_data, var_cases, "CSU_C")
setnames(dt_data, var_py, "CSU_P")
setnames(dt_data, var_year, "CSU_Y")

#group population (use sum)
dt_data <- dt_data[, list(CSU_C=sum(CSU_C),CSU_P=sum(CSU_P)), by=c("CSU_A", "CSU_Y") ]

#generate 5 year period

max_year <- max(dt_data$CSU_Y)
min_year <- min(dt_data$CSU_Y)
temp <- min_year+((max_year - min_year+1)%%5)
dt_data<- dt_data[CSU_Y >= temp,]
dt_data[, CSU_Y:=cut(CSU_Y,seq(temp,max_year+1,5 ), right = FALSE, labels = seq(temp+2,max_year+1,5 ))]
dt_data[, CSU_Y:=as.numeric(levels(CSU_Y))[CSU_Y]]



dt_data$CSU_Y
#group population (use sum)
dt_data <- dt_data[, list(CSU_C=sum(CSU_C),CSU_P=sum(CSU_P)), by=c("CSU_A", "CSU_Y") ]

#drop missing age
dt_data <- dt_data[dt_data$CSU_A!=missing_age] 

#create age dummy: 1 2 3 4 --- 18
dt_data$CSU_age_factor <- c(as.factor(dt_data$CSU_A))
dt_data[dt_data$CSU_P != 0,nb_age_group := max(CSU_age_factor)] 

#regroup case for population with nb of age group <  18 
for (i in 15:17) {
  if (i %in% dt_data$nb_age_group) {
    dt_data[dt_data$nb_age_group == i & dt_data$CSU_age_factor >= i , CSU_C:=sum(CSU_C), by="CSU_BY"] #add total_know
    dt_data[dt_data$nb_age_group == i & dt_data$CSU_age_factor > i & !is.na(dt_data$CSU_age_factor), CSU_C := 0] 
  } 
}

#create age label
dt_data[, temp_age:=(CSU_age_factor-1)*5]
dt_data[temp_age < max(temp_age),CSU_age_label:=paste0(temp_age, "-", temp_age+4)]
dt_data[temp_age == max(temp_age),CSU_age_label:=paste0(temp_age, "+")]

#create cohort year 
dt_data[,CSU_cohort:=CSU_Y - temp_age - 2.5]

#calcul rate 
dt_data$CSU_rate <- dt_data$CSU_C/dt_data$CSU_P *db_rate

# to calcul y axes breaks
tick <- .csu_tick_generator(max = max(dt_data$CSU_rate), min=min(dt_data[CSU_rate != 0,]$CSU_rate), log_scale = logscale )
tick_space <- tick$tick_list[length(tick$tick_list)] - tick$tick_list[length(tick$tick_list)-1]

#to calcul year axes break
year_tick <- .csu_year_tick_generator(min(dt_data$CSU_Y),max(dt_data$CSU_Y))


temp_top <- ceiling(max(dt_data$CSU_rate)/tick_space)*tick_space
temp_expand_y <- max(dt_data$CSU_rate)/35
temp_expand_y_up <- max(dt_data$CSU_rate)+temp_expand_y
if (temp_expand_y_up > temp_top-(tick_space/2)) {
  temp_expand_y_up <- temp_top+temp_expand_y
}

th_legend <- list(theme(legend.position="none"))

#format
if (!is.null(format_export)) {
  .csu_format_export(format_export, plot_title = plot_title)
}

#get color gradient
color_scale <- scales::seq_gradient_pal(low = "#60b1e2", high = "#0b3e6b")(seq(0,1,length.out=max_age))

dt_data$CSU_age_factor <- as.factor(dt_data$CSU_age_factor)



#csu_plot
if (logscale) {
  base_plot <- ggplot(dt_data[, CSU_rate := ifelse(CSU_rate==0,NA, CSU_rate )], aes(CSU_Y, CSU_rate))
} else {
  base_plot <- ggplot(dt_data, aes(CSU_Y, CSU_rate))
}



csu_plot <- base_plot+
  geom_line(aes(color=CSU_age_factor), size = 0.75,na.rm=TRUE)+
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
    scale_y_continuous(name = paste("Age-specific incidence rate per", formatC(db_rate, format="d")),
                       breaks=tick$tick_list,
                       minor_breaks = tick$tick_minor_list,
                       limits=c(tick$tick_list[1],tick$tick_list[length(tick$tick_list)]),
                       labels=.csu_axes_label,
                       trans = "log10"
    )
} else {
  
  csu_plot <- csu_plot +
    coord_cartesian( ylim=c(-temp_expand_y, temp_expand_y_up),  expand = TRUE)+
    scale_y_continuous(name = paste("Age-specific incidence rate per", formatC(db_rate, format="d")),
                       breaks=tick$tick_list,
                       labels=.csu_axes_label,
                       expand = c(0,0)
    )
}

csu_plot <- csu_plot + 
  geom_text(data = dt_data[CSU_Y == max(CSU_Y), ],
            aes(label = CSU_age_label),
            check_overlap = T,
            nudge_x=0.5,
            hjust=0)+
  theme(plot.margin = unit(c(0.5, legend$right_space_margin, 0.5, 0.5), "lines"))

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

csu_plot


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






