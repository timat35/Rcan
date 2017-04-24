
rcan_folder <- "c:/projects/Rcan"
#install.packages("http://timat.org/matR/Rcan.tar.gz", repos=NULL)
library(Rcan)
library(data.table)
library(ggplot2)
library(grid)

source(paste0(rcan_folder, "/csu_trend.r"))
source(paste0(rcan_folder, "/helper.r"))
source(paste0(rcan_folder, "/csu_trend_cohort_period.r"))



#test package
#detach(package:Rcan)
#remove.packages("Rcan")
#install.packages("C:/Projects/Rcan/Rcan_1.3.0.tar.gz", repos=NULL)


# create csu_period_cohort function 
data(csu_registry_data_2)
df_data <- csu_registry_data_2[csu_registry_data_2$registry == 84020,]
df_data <- df_data[df_data$sex==1,]

csu_trend_cohort_period(df_data=df_data,
                        missing_age =99,
                        plot_title = "test",
                        type = "Both",
                        format_export = "png",
                        logscale = TRUE)

 
  
  
















