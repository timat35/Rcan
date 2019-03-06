library(Rcan)
library(data.table)
# group ICD

var_age <- "age"
cross_by <- c("sex", "regcode")
group_by <- c("reglabel")
var_ICD <- "site"
var_year <- "doi"
var_cases <- NULL


data(ICD_group_file)
data(data_individual_file)

#group data by 
#	5 year age group 
df_data_age <- csu_group_cases(data_individual_file,
	var_age="age",
	cross_by=c("sex", "regcode", "site"),
	group_by=c("reglabel")) 

#group data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file
df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	cross_by=c("sex", "regcode"),
	group_by=c("reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site") 

#group data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file
#	year (extract from date of incidence)
df_data_year <- csu_group_cases(data_individual_file,
	var_age="age",
	cross_by=c("sex", "regcode"),
	group_by=c("reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site",
	var_year = "doi") 


write.csv(dt_result,"test.csv")
save(data_individual_file,file= "data_individual_file.rda")
save(ICD_group_file,file= "ICD_group_file.rda")
df_test <- readRDS("ICD_group_file.rda")

icd_group <- "C82-85,C96"

core.csu_icd_ungroup <- function(icd_group) {

	icd_group <- gsub("\\s", "", icd_group)
  
	icd_list <- NULL
	ICD_reg <-"(C\\d+)([^C+]?)(.+)?"

	while (nchar(icd_group)>=3) {

	  icd_start <- sub(ICD_reg, "\\1", icd_group)
	  icd_mark <- sub(ICD_reg, "\\2", icd_group)
	  icd_group <- sub(ICD_reg, "\\3", icd_group)


	  if (icd_mark == "-") {

	  	code_start <- sub("C(\\d+)", "\\1", icd_start)
	  	code_nchar <- nchar(code_start)
	  	code_start <- as.numeric(code_start)
	  	code_end <- as.numeric(sub("(\\d+)(.+)?", "\\1", icd_group))

	  	for (code in code_start:code_end) {
	  		icd_list <- c(icd_list, paste0("C", sprintf(paste0("%0",code_nchar,"d"), code)))
	  	}

	  	icd_group <- sub("(\\d+)([^C+]?)(.+)?", "\\3", icd_group) 
	  }
	  else  {
	  	icd_list <- c(icd_list, sub("(C\\d+)", "\\1", icd_start))
	  }
	}

	return(icd_list)
}
   


