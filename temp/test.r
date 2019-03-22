detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")

library(Rcan)
library(data.table)

#install.packages("rlang")





source("C:/Projects/Rcan/temp/function.r")
# group ICD


data(ICD_group_file)
data(data_individual_file)

df_data <- data_individual_file
var_age <- "age"
group_by <- c("sex", "regcode", "reglabel")
var_year <- "doi"

df_ICD <- NULL
var_ICD <- NULL
var_cases <- NULL


dt_test <- csu_group_cases(data_individual_file, var_age = "age",
                        group_by = c("sex", "regcode","reglabel"),
                       	df_ICD = ICD_group_file,
						var_ICD  ="site",
                        var_year = "doi")





#group data by 
#	5 year age group 

df_data_test <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "reglabel"))



#group data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file


dt_data <- csu_group_cases(data_individual_file,
         var_age="age",
         group_by=c("sex", "regcode","reglabel"),
         df_ICD = ICD_group_file,
         var_ICD  ="site") 


#group data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file
#	year (extract from date of incidence)

df_data_year_test <- csu_group_cases(data_individual_file,
                                var_age="age",
                                group_by=c("sex", "regcode","reglabel"),
                                df_ICD = ICD_group_file,
                                var_ICD  ="site",
                                var_year = "doi") 


write.csv(dt_result,"test.csv")

df <- read.csv("C:/Projects/Rcan/temp/data-individual_test.csv")
colnames(data_individual_file)  
data_individual_file<-df[,c("age","sex","regcode","reglabel","site","doi","histo","beh","basis")]

save(data_individual_file,file="data_individual_file.rda")
save(ICD_group_file,file= "ICD_group_file.rda")


df_test <- readRDS("ICD_group_file.rda")


ICD_group_file <- read.csv("C:/Projects/Rcan/temp/Datos-icd.csv")




