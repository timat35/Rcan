detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")



#jermoe test 

dcas <- read.csv("C:/Projects/Rcan/temp/Exercise2_qui.csv")
dpop <- read.csv("C:/Projects/Rcan/temp/200612_population.csv")

data("ICD_group_file")
dcases <- csu_group_cases(dcas, var_age = "age", group_by = c("sex"),df_ICD = ICD_group_file, var_ICD = "site", var_year = "doi")

names(dpop)[names(dpop) == 'agegrp'] <- 'age_group'
names(dcases)

df_cases <- dcases


dcaspop <- csu_merge_cases_pop(df_cases, dpop, var_age = "age_group", var_cases = "cases", var_py = "pop", group_by = c("sex", "LABEL", "year"))

#note population change to 85+
#missing pop MUST BE ZERO


library(Rcan)
library(data.table)
data(data_individual_file)
data(ICD_group_file)


ICD_group_file
getwd()
write.csv(ICD_group_file, )
ICD_group_file <- read.csv("C:/Projects/Rcan/temp/ICD_globocan.csv")
save(ICD_group_file, file = "C:/Projects/Rcan/Rcan/data/ICD_group_file.rda")

ICD_group_CI5XI <- read.csv("C:/Projects/Rcan/temp/ICD_CI5XI.csv")
save(ICD_group_CI5XI, file = "C:/Projects/Rcan/Rcan/data/ICD_group_CI5XI.rda")


setwd("C:/Projects/Rcan")
source("temp/function.r")



## add function for population 

##YEAR can be detect ok

df_pop <- read.csv("temp/BKKpop2013-2015.csv")
names(df_pop)[names(df_pop) == 'age'] <- 'age_group'

df_cases <- read.csv("temp/BKK2013-2015_cases.csv")
names(df_cases)[names(df_cases) == 'Sex'] <- 'sex_code'

help(csu_merge_cases_pop)

df_final <- csu_merge_cases_pop (
	df_cases,
	df_pop,
	"age_group", 
	"cases",
	group_by="sex_code")

View(df_final)

## test with other data 
source("temp/function.r")
df_cases <- read.csv("temp/0-Manipulation/Exercise0.csv")
data(ICD_group_file)

# no year data
df_cases <- csu_group_cases(df_cases, 
	"AGE",
	group_by="SEX",
	df_ICD = ICD_group_file,
	var_ICD="I10"
) 

head(df_cases)

df_pop <- read.table("temp/0-Manipulation/Exercise0-population.txt", header=TRUE)
names(df_pop)[names(df_pop) == 'age'] <- 'age_group'
names(df_pop)[names(df_pop) == 'sex'] <- 'SEX'
head(df_pop)
tail(df_cases)

var_age <- "age_group"
var_cases <- "cases"
var_py <- "pop"
group_by <- "SEX"


df_final <- csu_merge_cases_pop (
	df_cases,
	df_pop,
	"age_group", 
	"cases",
	"pop", 
	group_by="SEX")

View(df_final)

## test with year in data and population


source("temp/function.r")
df_cases <- read.csv("temp/cases1.csv")
data(ICD_group_file)
colnames(df_cases)
head(df_cases)
# no year data
df_cases <- csu_group_cases(df_cases, 
	"AGE",
	group_by="SEX",
	df_ICD = ICD_group_file,
	var_ICD="I10", 
	var_year = "INCID"
) 

head(df_cases)
df_pop <- read.csv("temp/3-Manipulacion/Exercise3-population.csv")
names(df_pop)[names(df_pop) == 'age'] <- 'age_group'
names(df_pop)[names(df_pop) == 'sex'] <- 'SEX'
head(df_pop)

var_age <- "age_group"
var_cases <- "cases"
var_py <- "pop"
group_by <- "SEX"


df_final <- csu_merge_cases_pop (
	df_cases,
	df_pop,
	"age_group", 
	"cases",
	"pop", 
	group_by="SEX")

View(df_final)

## test with year in data and but not in population
source("temp/function.r")
df_cases <- read.csv("temp/0-Manipulation/Exercise0.csv")
data(ICD_group_file)
head(df_cases)
# no year data
df_cases <- csu_group_cases(df_cases, 
	"AGE",
	group_by="SEX",
	df_ICD = ICD_group_file,
	var_ICD="I10", 
	var_year = "INCID"
) 

head(df_cases)

df_pop <- read.table("temp/0-Manipulation/Exercise0-population.txt", header=TRUE)
names(df_pop)[names(df_pop) == 'age'] <- 'age_group'
names(df_pop)[names(df_pop) == 'sex'] <- 'SEX'
head(df_pop)
tail(df_cases)

var_age <- "age_group"
var_cases <- "cases"
var_py <- "pop"
group_by <- "SEX"


df_final <- csu_merge_cases_pop (
	df_cases,
	df_pop,
	"age_group", 
	"cases",
	"pop", 
	group_by="SEX")

View(df_final)

##last test  ? 
