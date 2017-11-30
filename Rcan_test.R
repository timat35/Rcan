
#create dataset 

library(data.table)
app_folder <- "C:/Projects/Rcan/Shiny"
csu_CI5X_data <- data.table(readRDS(paste0(app_folder, "/data/CI5X.rds")))
setnames(csu_CI5X_data, "cancer_lab", "cancer_label")
setnames(csu_CI5X_data, "registry_lab", "registry_label")
setnames(csu_CI5X_data, "cancer", "cancer_code")
setnames(csu_CI5X_data, "registry", "registry_code")
csu_CI5X_data$sex <- factor(csu_CI5X_data$sex, levels=c(1,2), labels =c("Male", "Female"))
save(csu_CI5X_data, file="csu_CI5X_data.rda")

#CI5 comparaison add checking variable

rcan_folder <- "c:/Projects/Rcan"

#install.packages(c("ggplot2", "data.table"))
install.packages("http://timat.org/matR/Rcan.tar.gz", repos=NULL)

library(Rcan)
library(data.table)
library(ggplot2)
library(grid)



rcan_folder <- "c:/Projects/Rcan"
source(paste0(rcan_folder, "/Rcan/R/helper.r"))
source(paste0(rcan_folder, "/Rcan/R/csu_ageSpecific.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_trend.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_eapc.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_asr.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_trend_legend.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_trendCohortPeriod.r"))

getOption("repos")

#test package
detach(package:Rcan)
remove.packages("Rcan")
install.packages("C:/Projects/Rcan/Rcan_1.3.1.tar.gz", repos=NULL)

library(Rcan)
test_package("Rcan")
help(Rcan)
csu_eapc
remove.packages("ggplot2")

tempdir()

help(tools)
# example age specific ---------

data(csu_registry_data_1)
data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# to select only 1 population.
test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

# plot age specific rate for 1 population.
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male")

# plot age specific rate for 1 population, and comparison with CI5X data.
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male",
                CI5_comparison = "Liver")

# plot age specific rate for 4 population, legend at the bottom and comparison with CI5X data.
csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(position="bottom", nrow = 1),
                plot_title = "Liver, male",
                CI5_comparison = 7)

# plot age specific rate for 4 population, legend at the right.
csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(position="right", right_space_margin = 6.5),
                plot_title = "Liver, male")	

#plot logscale with CI5 data
temp <- csu_ageSpecific(csu_registry_data_1,
                log_scale = TRUE,
                group_by="registry_label",
                legend=csu_trend_legend(position="bottom", nrow = 1),
                plot_title = "Liver, male",
                CI5_comparison = 7)



# Plot embedded in a graphic device
pdf("test.pdf",width = 11.692 , height =  8.267) 


csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(position="bottom", nrow = 2),
                plot_title = "Liver, male",
                CI5_comparison = 7)

plot.new()

csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(position="right", right_space_margin = 6.5),
                plot_title = "Liver, male")	



dev.off()


# example age specific top

library(Rcan)
data("csu_CI5X_data")

#get the registry code asssociate to registry_label
print(unique(csu_CI5X_data[,c("registry_label", "registry_code")]),nrows = 1000)

#get the cancer code asssociate to cancer_label
print(unique(csu_CI5X_data[,c("cancer_label", "cancer_code")]),nrows = 1000)

#remove all cancers:
df_data <- csu_CI5X_data[csu_CI5X_data$cancer_code < 62,]


#select Thailand changmai
df_data_1 <- df_data[df_data$registry_code==76401,]


#select USAm NPCR
df_data_2 <- df_data[df_data$registry_code== 84080,]


# plot for Thailand Changmai
dt_result_1 <- csu_ageSpecific_top(df_data_1,
                    var_age="age", 
                    var_cases="cases", 
                    var_py="py",
                    var_top="cancer_label", 
                    group_by="sex", 
                    plot_title= "Thailand, Chiangmai",
                    plot_subtitle = "Top 5 cancer",
                    missing_age = 19)


# plot for USA NPCR
dt_result_2 <- csu_ageSpecific_top(df_data_2,
                                   var_age="age", 
                                   var_cases="cases", 
                                   var_py="py",
                                   var_top="cancer_label", 
                                   group_by="sex", 
                                   plot_title= "USA, NPCR",
                                   plot_subtitle = "Top 5 cancer",
                                   missing_age = 19)




# example time trend 


data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# to select only 1 population 
test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]

# to change sex variable to factor with label
test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))

# to calculate the asr
df_asr <- csu_asr(test,missing_age = 99,
                  group_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))


# plot ASR ove year, by sex.
csu_time_trend(df_asr, group_by="sex",
          plot_title = "Colombia, Liver")

# plot ASR over year, by sex, with small smoothing.
csu_time_trend(df_asr, group_by="sex",
          plot_title = "Colombia, Liver",
          smoothing = 0.3)

# plot ASR over year, by sex, with high smoothing.
csu_time_trend(df_asr, group_by="sex",
          plot_title = "Colombia, Liver",
          smoothing = 0.5)

# Plot embedded in a graphic device
pdf("example_test.pdf")
csu_time_trend(df_asr, group_by="sex",
          plot_title = "Colombia, Liver",
          smoothing = 0.3)

csu_time_trend(df_asr, group_by="sex",
          plot_title = "Colombia, Liver",
          smoothing = 0.5)

dev.off()


getwd()

