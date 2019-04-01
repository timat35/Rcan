#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")

library(Rcan)
library(data.table)
data(data_individual_file)

setwd("C:/Projects/Rcan")
source("temp/function.r")

## add function for population 

##YEAR can be detect ok

df_pop <- read.csv("temp/BKKpop2013-2015.csv")
names(df_pop)[names(df_pop) == 'age'] <- 'age_group'

df_cases <- read.csv("temp/BKK2013-2015_cases.csv")
names(df_cases)[names(df_cases) == 'Sex'] <- 'sex_code'

View(df_pop)

df_final <- csu_merge_cases_pop (
	df_cases,
	df_pop,
	"age_group", 
	"cases",
	"py", 
	group_by="sex_code")


