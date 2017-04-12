
rcan_folder <- "c:/projects/Rcan"
library(Rcan)

source(paste0(rcan_folder, "/csu_asr.r"))

# create csu_time_trend function 
df_data <- data("csu_registry_data_2")

df_data <- csu_registry_data_2
df_asr <- csu_asr(df_data = df_data,missing_age = 99,
                  var_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))


