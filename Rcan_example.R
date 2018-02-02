library(Rcan)

data(csu_registry_data_2)


# ASR with standard error with missing age.
df_asr <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err")

df_asr[1:4,]	

# Truncated ASR, 25-69 years.
result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
                  first_age = 6, 
                  last_age = 14,
                  missing_age = 99)	

result[1:4,]	


# EAPC with standard error		
df_eapc <- csu_eapc(df_asr,
                    "asr", "year",
                    group_by=c("registry", "registry_label", "sex", "ethnic" ))

df_eapc[1:4,]	



data(csu_registry_data_1)
# plot age specific rate for 1 population.
df_colombia <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

csu_ageSpecific(df_colombia,
                plot_title = "Colombia, Liver, male")

# plot age specific rate for 1 population, and comparison with CI5X data.
csu_ageSpecific(df_colombia,
                plot_title = "Colombia, Liver, male",
                CI5_comparison = "Liver")

# plot age specific rate for 4 population, legend at the bottom and comparison with CI5X data.
csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                logscale = TRUE,
                legend=csu_trend_legend(position="right", right_space_margin = 6.5),
                plot_title = "Liver, male")


# age specific top 5
data("csu_CI5X_data")

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
                                   logscale=TRUE,
                                   var_top="cancer_label", 
                                   group_by="sex", 
                                   plot_title= "USA, NPCR",
                                   plot_subtitle = "Top 5 cancer",
                                   missing_age = 19)


#cohort period graph
data(csu_registry_data_2)

# to select only 1 population 
test <- csu_registry_data_2[csu_registry_data_2$registry == 84020,]
test <- test[test$sex==1,]

# plot cohort graph from 25-29 years until 75-79 years.
csu_trendCohortPeriod(df_data=test,
                      missing_age =99,
                      plot_title = "USA, Liver, males")

# plot Period graph from 0-5 until 85+.
csu_trendCohortPeriod(df_data=test,
                      missing_age =99,
                      plot_title = "USA, Liver, males",
                      type="Period",
                      first_age=1,
                      last_age=18)

# plot Cohort-Period graph from 30-64 years until 70-74 years.
csu_trendCohortPeriod(df_data=test,
                      missing_age =99,
                      plot_title = "USA, Liver, males",
                      type="Both",
                      first_age=7,
                      last_age=15)

# plot Cohort-Period graph from 30-64 years until 70-74 years with Y axis normal scale.
csu_trendCohortPeriod(df_data=test,
                      missing_age =99,
                      plot_title = "USA, Liver, males",
                      type="Both",
                      first_age=7,
                      last_age=15,
                      logscale=FALSE)

# plot Cohort graph from 25-29 years until 75-79 years, with data grouped in 2 years period.
csu_trendCohortPeriod(df_data=test,
                      missing_age =99,
                      plot_title = "USA, Liver, males",
                      type="Cohort",
                      year_group = 2)
