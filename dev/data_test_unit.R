
library(testthat)
library(vdiffr)
library(ggplot2)
library(data.table)
library(Rcan)
library(grid)



install.packages("devtools")

pkg_folder <- ("c:/Projects/Rcan/Rcan")

library(devtools)


devtools::test(pkg_folder)




# create expect reusult for csu_asr 1
data_test <-  read.csv(paste0(rcan_folder,"/data_test/data_cervix.csv"), sep=",")
df_asr <- csu_asr_new(data_test,missing_age = 19,
                  group_by  = c("country", "country_label", "year", "sex","type"),
                  var_age_group =  c("country", "country_label","type"))

saveRDS(df_asr, paste0(pkg_folder,"/tests/testthat/csu_asr_test1.rds"))


#create expect for csu_graph_ageSpecific 1
data(csu_registry_data_1)
data(csu_registry_data_2)

data_test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]
test <- csu_ageSpecific(data_test,plot_title = "Colombia, Liver, male")
saveRDS(test, paste0(pkg_folder,"/tests/testthat/csu_graph_ageSpecific_test1.rds"))

#create expect for csu_trend 1
data(csu_registry_data_2)


# to select only 1 population 
test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]

# to change sex variable to factor with label
test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))

# to calculate the asr
df_asr <- csu_asr(test,missing_age = 99,
                  group_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))


# plot ASR over year, by sex, with small smoothing.
temp <- csu_time_trend(df_asr, group_by="sex",logscale=TRUE,
          plot_title = "Colombia, Live",
          smoothing = 0.3)

saveRDS(temp, paste0(pkg_folder,"/tests/testthat/csu_trend_test1.rds"))


#create expect for csu_trend 2

test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]

# to change sex variable to factor with label
test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))

# to calculate the asr
df_asr <- csu_asr(test,missing_age = 99,
                  group_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))


# plot ASR over year, by sex, with small smoothing.
temp <- csu_time_trend(df_asr, group_by="sex",logscale=TRUE,
                       plot_title = "Colombia, Live",
                       smoothing = 0.3)


#create expect for csu_eapc 1

data(csu_registry_data_2)

df_asr <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  missing_age = 99)

temp <- csu_eapc(df_asr,
                   "asr", "year",
                   group_by=c("registry", "registry_label", "sex", "ethnic" ))


saveRDS(temp, paste0(pkg_folder,"/tests/testthat/csu_eapc_test1.rds"))


#create expect for csu_eapc 2
data(csu_registry_data_2)

df_asr <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  missing_age = 99)

temp <- csu_eapc(df_asr,
                 "asr", "year",
                 group_by=c("registry", "registry_label", "ethnic" ))

