
library(testthat)
library(ggplot2)
library(data.table)


rcan_folder <- "c:/Projects/Rcan"
source(paste0(rcan_folder, "/Rcan/R/csu_asr.r"))

test_results <- test_dir(paste0(rcan_folder, "/test"), reporter="summary")




# create expect reusult for csu_asr 1
data_test <-  read.csv(paste0(rcan_folder,"/data_test/data_cervix.csv"), sep=",")
df_asr <- csu_asr(data_test,missing_age = 19,
                  group_by  = c("country", "country_label", "year", "sex","type"),
                  var_age_group =  c("country", "country_label","type"))

saveRDS(df_asr, paste0(rcan_folder, "/test/csu_asr_test1.rds"))
