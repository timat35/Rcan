library(Rcan)
library(data.table)
# group ICD

var_age <- "AGE"
cross_by <- "SEX"
var_ICD <- "I10"
var_year <- "INCID"
var_cases <- NULL
group_by <- NULL

setwd("c:/projects/Rcan/temp")

df_ICD <- read.csv("Datos-icd.csv")
dt_ICD <- data.table(df_ICD)
setkeyv(dt_ICD,c("LABEL", "ICD")) 

dt_ICD <- unique(dt_ICD)
dt_table <- data.table()

cancer <- "COLORECTUM"

for (cancer in levels(dt_ICD$LABEL)) {
	icd_group <- dt_ICD[LABEL == cancer, ]$ICD
	temp <- paste(icd_group, collapse=",")
	temp <- core.csu_icd_ungroup(temp)
	temp <- data.table(ICD_ungroup = temp, ICD =icd_group )
	dt_table <- rbind(dt_table, temp)
}

dt_ICD <- merge(dt_ICD, dt_table, by="ICD")


dt_ICD[, ICD_group:= sapply(LABEL, function(x) {Rcan:::core.csu_icd_group(as.vector(dt_ICD[LABEL == x, ]$ICD))})]
dt_ICD[, ICD:=ICD_group]
dt_ICD[, ICD_group:=NULL]
dt_ICD <- unique(dt_ICD)

dt_table <- data.table()
for (cancer in levels(dt_ICD$LABEL)) {
	icd_group <- dt_ICD[LABEL == cancer, ]$ICD
	test <- core.csu_icd_ungroup(icd_group)
	test <- data.table(ICD_ungroup = test, ICD =icd_group )
	dt_table <- rbind(dt_table, test)
}

dt_ICD <- merge(dt_ICD, dt_table, by="ICD")

dt_ICD[, ICD:= sapply(LABEL, function(x) {core.csu_icd_ungroup(as.vector(dt_ICD[LABEL == x, ]$ICD_GROUP))})]


df_data <- read.csv("1-Verificacion/Ejercicio1.csv")
df_data[1:5,]

ICD_group_file <- read.csv("Datos-icd.csv")


dt_result <- csu_group_cases(df_data,
	var_age="age",
	cross_by=c("sex"),
	df_ICD = df_ICD,
	var_ICD  ="site",
	var_year = "doi") 


write.csv(dt_result,"test.csv")
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
   


