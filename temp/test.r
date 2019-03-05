
# group ICD
library(data.table)
setwd("c:/projects/Rcan/temp")
source("function.r")

df_data <- read.csv("cases1.csv")
var_age <- "AGE"
cross_by <- "SEX"
df_ICD <- read.csv("Datos-icd.csv")
var_ICD <- "I10"
var_year <- "INCID"
var_cases <- NULL
group_by <- NULL

dt_result <- data_group(
	df_data,
	var_age ,
	cross_by,
	df_ICD = df_ICD,
	var_ICD  =var_ICD,
	var_year = var_year) 






individual file:
regoup by AGE
regroup by ICD (optional)
regroup by YEAR (optional)

individual file
var age 

cross_by (all group possibility)
other group variable to keep (label for instance)

ICD file, var ICD 

var year 



