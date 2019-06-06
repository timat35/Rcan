
#install dependant packages...

install.packages("data.table")
install.packages("ggplot2")
install.packages("grid")
install.packages("scales")
install.packages("testthat")
install.packages("devtools")
install.packages("digest")


#update package (last stable version)
detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", subdir="Rcan")

#update package (dev version)
detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")

#run test unit
library(Rcan)
library("testthat")
test_dir("C:/Projects/Rcan/Rcan/tests/testthat")

#check packages: (check need Roxygene) and built

library(devtools)
library(spelling)


check("C:/Projects/Rcan/Rcan") #local check
check_man("C:/Projects/Rcan/Rcan") #local check documentation
build("C:/Projects/Rcan/Rcan", path="C:/Projects/Rcan/release", manual = TRUE) # build package
build("C:/Projects/Rcan/Rcan",path="C:/Projects/canreg5/conf/tables/r/r-packages", manual = TRUE) # build package for canreg

# post package on CRAN

spell_check_package("C:/Projects/Rcan/Rcan")
check_rhub("C:/Projects/Rcan/Rcan")
check_win_devel("C:/Projects/Rcan/Rcan") #build win check
release_checks("C:/Projects/Rcan/Rcan")
release("C:/Projects/Rcan/Rcan", check=FALSE)

#create dataset from shiny app

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



library(Rcan)
library(data.table)
library(ggplot2)
library(grid)


source(paste0(rcan_folder, "/Rcan/R/helper.r"))
source(paste0(rcan_folder, "/Rcan/R/csu_ageSpecific.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_trend.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_eapc.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_asr.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_trend_legend.r"))
#source(paste0(rcan_folder, "/Rcan/R/csu_trendCohortPeriod.r"))

getOption("repos")

library(data.table)

# create CI5 mean data for age specific function 
View(CI5_base)  

CI5_base <- as.data.table(read.csv("C:/Data/CI5XI/CI5XI_country.csv"))

setkeyv(CI5_base, c("country_label", "cancer","sex", "age"))
#select country with population at 85+ 
CI5_base <- CI5_base[age < 19,]
CI5_base[,temp:=min(py), by=country_code]
CI5_base <- CI5_base[temp > 0,]
CI5_base <- CI5_base[,.(CSU_C=sum(cases), CSU_P=sum(py)), by=c("cancer", "cancer_lab", "age")]

setnames(CI5_base, "cancer_lab", "ci5_cancer_label")
setnames(CI5_base, "cancer", "ci5_cancer_code")
setnames(CI5_base, "age", "CSU_age_factor")

# drop some cancer sites
CI5_base <- CI5_base[!ci5_cancer_code %in% c(22,34,36,41,46,61),]

#rename some cancer site
CI5_base[ci5_cancer_code == 17,ci5_cancer_label := "Gallbladder"]
CI5_base[ci5_cancer_code == 19,ci5_cancer_label := "Nose, sinuses"]
CI5_base[ci5_cancer_code == 21,ci5_cancer_label := "Lung"]

write.csv(unique(CI5_base[, c("ci5_cancer_code", "ci5_cancer_label"), with=FALSE]), "temp.csv",row.names=FALSE )
nrow(CI5_base)

csu_ci5_mean <- CI5_base
save(csu_ci5_mean, file = "csu_ci5_mean.rda")

csu_CI5XI_data <- as.data.table(readRDS("C:/Data/CI5XI/CI5XI.rds"))

csu_CI5XI_data$notes <- NULL
csu_CI5XI_data$total <- NULL
csu_CI5XI_data$n_agr <- NULL
nrow(csu_CI5XI_data)
setnames(csu_CI5XI_data, "registry", "registry_code")
setnames(csu_CI5XI_data, "registry_lab", "registry_label")
setnames(csu_CI5XI_data, "cancer", "cancer_code")
setnames(csu_CI5XI_data, "cancer_lab", "cancer_label")

csu_CI5XI_data[, CI5_continent:=as.integer(CI5_continent)]
csu_CI5XI_data[, ethnic_group:=as.integer(ethnic_group)]

str(csu_CI5XI_data)
View(csu_CI5XI_data)  

save(csu_CI5XI_data, file = "csu_CI5XI_data.rda")
# example csu_cases_group--------

  data(ICD_group_GLOBOCAN)
  data(data_individual_file)

  #group individual data by 
  # 5 year age group 
  df_data_age <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode", "site")) 

  #group individual data by 
  # 5 year age group 
  # ICD grouping from dataframe ICD_group_GLOBOCAN

  df_data_icd <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode"),
    df_ICD = ICD_group_GLOBOCAN,
    var_ICD  ="site") 

  #group individual data by 
  # 5 year age group 
  # ICD grouping from dataframe ICD_group_GLOBOCAN
  # year (extract from date of incidence)

  df_data_year <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode"),
    df_ICD = ICD_group_GLOBOCAN,
    var_ICD  ="site",
    var_year = "doi")       


# example csu_pop_cases--------


  data(ICD_group_GLOBOCAN)
  data(data_individual_file)



  #group individual data by 
  # 5 year age group 
  # ICD grouping from dataframe ICD_group_GLOBOCAN
  # year (extract from date of incidence)

  df_data_year <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode"),
    df_ICD = ICD_group_GLOBOCAN,
    var_ICD  ="site",
    var_year = "doi")     

  #Merge 5-years age grouped data with population by year (automatic) and sex

  df_pop <- csu_merge_cases_pop(df_data_year, data_population_file, var_age = "age_group", 
                             var_cases = "cases", var_py = "pop", group_by = c("sex"))


#example csu bar top 


  data(ICD_group_GLOBOCAN)
  data(data_individual_file)
  data(data_population_file)
  load("data_individual_file.rda")
  load("data_population_file.rda")

  #group individual data by 
  # 5 year age group 
  # ICD grouping from dataframe ICD_group_GLOBOCAN
  # year (extract from date of incidence)

  df_data_year <- csu_group_cases(data_individual_file,
    var_age="age",
    group_by=c("sex", "regcode"),
    df_ICD = ICD_group_GLOBOCAN,
    var_ICD  ="site",
    var_year = "doi")     

  #Merge 5-years age grouped data with population by year (automatic) and sex

  df_data <- csu_merge_cases_pop(
    df_data_year, 
    data_population_file, 
    var_age = "age_group",
    var_cases = "cases",
    var_py = "pop",
    group_by = c("sex"))

  # calculate asr
  df_asr <- csu_asr(df_pop,
    "age_group", 
    "cases",
    "pop",
    group_by=c("sex", "ICD_group", "LABEL"),
    missing_age =19)

  #remove Other cancer
  df_asr <- df_asr[!df_asr$LABEL %in% c("Other", "Other skin"), ]

  #keep male
  df_asr_male <- df_asr[df_asr$sex==1,]


  #Single sided bar plot 
  data1 <- csu_bar_top(
     df_asr_male,
     var_value="cases",
     var_bar="LABEL",
     nb_top = 10,
     plot_title = "Top 10 cancer sites",
     xtitle= "Number of cases",
     color= c("#2c7bb6"),
     digits=0) 

  #Double sided bar plot example 1
  data2 <- csu_bar_top(
     df_asr,
     var_value="cases",
     var_bar="LABEL",
     group_by="sex",
     nb_top = 15,
     plot_title = "Top 15 cancer sites",
     xtitle= "Number of cases",
     label_by=c("Male", "Female"),
     color = c("#2c7bb6","#b62ca1"),
     digits=0) 

  #Double sided bar plot example 2
  data2 <- csu_bar_top(
     df_asr,
     var_value="asr",
     var_bar="LABEL",
     group_by="sex",
     nb_top = 10,
     plot_title = "Top 10 cancer sites",
     xtitle= "Age-standardized rate per 100,00",
     label_by=c("Male", "Female"),
     color = c("#2c7bb6","#b62ca1"),
     digits=1) 



  # example bar top

  data(data_individual_file)
  data(csu_registry_data_2)

  # you can import your data from csv file using read.csv:
  # mydata <-  read.csv("mydata.csv", sep=",")
     
  # ASR
  result <- csu_asr(csu_registry_data_1, 
                    "age", "cases", "py",
                    group_by = c("registry", "registry_label" ),
                    var_age_group = c("registry_label"))

  result <- result[]

  df_asr_male <- df_asr[dcasasr$sex==1,]


  data <- csu_bar_top(
     dcasasr_male,
     var_value="cases",
     var_bar="LABEL",
     nb_top = 10,
     plot_title = "Top 10 cancer sites",
     xtitle= "Number of cases",
     color= c("#2c7bb6"),
     digits=0) 

  color_test <- read.csv("temp_color.csv")
  dcasasr_male <- merge(dcasasr_male, color_test, by="LABEL")

  data <- csu_bar_top(
     dcasasr_male,
     var_value="cases",
     var_bar="LABEL",
     nb_top = 10,
     plot_title = "Top 10 cancer sites",
     xtitle= "Number of cases",
     color="hex_color",
     digits=0) 



  data2 <- csu_bar_top(
     dcasasr,
     var_value="cases",
     var_bar="LABEL",
     group_by="sex",
     nb_top = 15,
     plot_title = "Top 15 cancer sites",
     xtitle= "Number of cases",
     label_by=c("Male", "Female"),
     color = c("#2c7bb6","#b62ca1"),
     digits=0) 

  data2 <- csu_bar_top(
     dcasasr,
     var_value="asr",
     var_bar="LABEL",
     group_by="sex",
     nb_top = 10,
     plot_title = "Top 10 cancer sites",
     xtitle= "Age-standardized rate per 100,00",
     label_by=c("Male", "Female"),
     color = c("#2c7bb6","#b62ca1"),
     digits=1)








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


#----- example age specific top ------

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

  # plot for USA NPCR with color


  #associate cancer with color


  dt_cancer_color <- data.table(read.csv(paste0(rcan_folder, "/data_test/color_cancer.csv")))
  setnames(dt_cancer_color, "cancer", "cancer_code")
  setnames(dt_cancer_color, "cancer_lab", "cancer_label")
  df_data_2 <- merge(df_data_2, dt_cancer_color, by=c("cancer_label", "cancer_code"))
  dt_result_2 <- csu_ageSpecific_top(df_data_2,
                                     var_age="age", 
                                     var_cases="cases", 
                                     var_py="py",
                                     var_top="cancer_label", 
                                     group_by="sex", 
                                     var_color="cancer_color",
                                     plot_title= "USA, NPCR",
                                     plot_subtitle = "Top 5 cancer",
                                     missing_age = 19)




# example time trend -----

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
            legend=csu_trend_legend(position="right", right_space_margin = 6.5),
            smoothing = 0.5)

  dev.off()


  getwd()



  data(csu_registry_data_1)
  data(csu_registry_data_2)

  # you can import your data from csv file using read.csv:
  # mydata <-  read.csv("mydata.csv", sep=",")

  # Age standardized rate (ASR) with no missing age cases.
  result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))
          
  # you can export your result as csv file using write.csv:
  # write.csv(result, file="result.csv")
          
  # ASR,  with the percentage of correction due to missing age cases. 
  result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"),
          missing_age = 19,         
          correction_info = TRUE) 
          
  # ASR and standard error with missing age.
  result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
          missing_age = 99)
          
  # Truncated ASR, 25-69 years.
  result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
          first_age = 6, 
          last_age = 14,
          missing_age = 99) 

  # Truncated ASR, 0-15 with denominator population = 1000000. 
  result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
          first_age = 1, 
          last_age = 3, 
          missing_age = 99, 
          db_rate = 1000000)    
          
  # ASR with EURO population as reference (instead of SEGI)
  result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"),
          missing_age = 19,
                  pop_base = "EURO")



  regex_year  <- "(18|19|20)\\d{2}"
  test <- c("sex", "coucou2012", "couqui2013", "age")
  bool_long <- any(grepl(regex_year,test))
  col_year <- test[grepl(regex_year,test)]