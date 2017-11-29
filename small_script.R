

library(data.table)
library(ggplot2)
library(testthat)
library(Rcan)
test_package("Rcan")


df_CI5 <- read.csv("c:/data/CI5X/CI5X.csv")
saveRDS(df_CI5, "c:/data/CI5X/CI5X.rds")



rcan_folder <- "c:/Projects/Rcan"
source(paste0(rcan_folder, "/Rcan/R/helper.r"))
source(paste0(rcan_folder, "/Rcan/R/csu_ageSpecific_top.r"))

data("csu_registry_data_2")
df_test <- csu_registry_data_2

df_test$sex <- factor(df_test$sex , levels=c(1,2), labels = c("Male", "Female"))


test <-csu_ageSpecific_top(
  df_data=df_test,
  var_age="age", 
  var_cases= "cases", 
  var_py= "py",
  var_top="registry_label",
  group_by="sex",
  missing_age=99, 
  logscale = TRUE,
  nb_top = 5,
  plot_title="Rcan test",
  plot_subtitle="Top 5") 


temp <- plotfinal$dt_data

plotfinal$dataall

test <- unique(plotfinal$dataall[,c("dummy_top","sex", "registry_label", "CSU_RANK"), with=FALSE])

dt_temp <- merge(plotfinal$datalist[[1]], test, by=c("dummy_top","sex"), all.x=TRUE, all.y=FALSE)


csu_registry_data_2 <- core.csu_dt_rank(csu_registry_data_2, var_value = "cases", var_rank = "registry_label",group_by = "sex", number = 5) 
csu_registry_data_2[["registry_label"]] <-core.csu_legend_wrapper(csu_registry_data_2[["registry_label"]], 14)
csu_registry_data_2[, temp:=as.factor("sex")]
