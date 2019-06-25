#DATA TABLE is DATA TABLE AT THE BEGINNING


#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", subdir="Rcan")

setwd("C:/Projects/Rcan/temp")



data(csu_registry_data_2)

  # you can import your data from csv file using read.csv:
  # mydata <-  read.csv("mydata.csv", sep=",")
  
  # to select only 1 population 
  test <- subset(csu_registry_data_2,registry == 84020 & sex == 1)


  
  # plot cohort graph from 25-29 years until 75-79 years.
  csu_trendCohortPeriod(df_data=test,
                        plot_title = "USA, Liver, males")



  data(data_individual_file)
  data(data_population_file)
  data(ICD_group_GLOBOCAN)

source("function.r")
df_data_year <- core.csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site",
  var_year = "doi", 
  var_cases = NULL,
  all_cancer=FALSE
  )     

head(df_data_year)