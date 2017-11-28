rcan_folder <- "c:/Projects/Rcan"
source(paste0(rcan_folder, "/Rcan/R/csu_eapc.r"))
library(data.table)
library(ggplot2)

library(testthat)
library(Rcan)
test_package("Rcan")


df_CI5 <- read.csv("c:/data/CI5X/CI5X.csv")
saveRDS(df_CI5, "c:/data/CI5X/CI5X.rds")


source(paste0(rcan_folder, "/Rcan/R/helper.r"))




test <- core.csu_ageSpecific_top(dt=csu_registry_data_2,
                                 var_age="age", 
                                 var_cases= "cases", 
                                 var_py= "py",
                                 group_by="sex",
                                 missing_age=99, 
                                 var_top="registry_label",
                                 logscale = FALSE,
                                 nb_top = 5,
                                 plot_title="test",
                                 var_age_label_list = NULL,
                                 var_color=NULL) 

csu_registry_data_2 <- core.csu_dt_rank(csu_registry_data_2, var_value = "cases", var_rank = "registry_label",group_by = "sex", number = 5) 
csu_registry_data_2[["registry_label"]] <-core.csu_legend_wrapper(csu_registry_data_2[["registry_label"]], 14)
csu_registry_data_2[, temp:=as.factor("sex")]
