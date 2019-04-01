#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")



#


library(Rcan)
library(data.table)


data(data_individual_file)



setwd("C:/Projects/Rcan")

## add function for population 

##YEAR can be detect ok

df_pop <- read.csv("temp/BKKpop2013-2015.csv")
names(df_pop)[names(df_pop) == 'age'] <- 'age_group'

df_cases <- read.csv("temp/BKK2013-2015_cases.csv")
names(df_cases)[names(df_cases) == 'Sex'] <- 'sex_code'

group_by = c("sex_code")
var_age = "age_group"
var_cases = "cases"
var_py = "py"

dt_pop <- as.data.table(df_pop)
dt_cases <- as.data.table(df_cases)

bool_year <- FALSE
bool_temp <- FALSE

regex_year  <- "(18|19|20)\\d{2}"

bool_long <- any(grepl(regex_year,colnames(dt_pop)))
if (bool_long) {

	bool_year <- TRUE
	dt_pop <- melt(dt_pop, c(group_by,var_age), patterns(regex_year), "year", "py")
	dt_pop[, year:= as.numeric(gsub(".*?((?:18|19|20)\\d{2}).*$", "\\1", year, perl=TRUE))]
} else {
	for (colyear_pop in colnames(dt_pop)[!colnames(dt_pop) %in% c(var_age,var_py)]) {

		bool_wide = (all(grepl(regex_year,unique(dt_pop[[colyear_pop]]))))

		if (bool_wide) {
			bool_year <- TRUE
			break
		}
	}
	if (bool_year) {
		setnames(dt_pop, colyear_pop, "year")
		dt_pop, [year:=as.numric(year)]
	}
}



dt_pop[,c(var_age) :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",get(var_age), perl=TRUE))]

if (max(dt_pop[[var_age]]) > 25) {
	dt_pop[,c(var_age) := round((get(var_age)/5)+1)]
}

dt_pop[get(var_age) >18, c(var_age) := 18 ]
dt_pop <- dt_pop[, .(py = sum(py)), by=c(group_by,var_age, "year")]

dt_cases[,c(var_age) :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",get(var_age), perl=TRUE))]
if (max(dt_cases[[var_age]]) > 25) {
	dt_cases[,c(var_age) := round((get(var_age)/5)+1)]
}


merge_col <- c(group_by, var_age)
if (bool_year) {
	for (colyear in colnames(dt_cases)[!colnames(dt_cases) %in% c(var_age,var_cases)]) {
		bool_temp = (all(grepl(regex_year,unique(dt_cases[[colyear]]))))
		if (bool_temp) {
			setnames(dt_cases, colyear, "year")
			merge_col <- c(merge_col, "year")
			break
		}
	}
	if (!bool_temp) {
		stop(paste0("Population data have a variable year: ",colyear_pop,"\nCases data do not have any year variable\n 
      	see documentation: Help(", deparse(substitute(csu_merge_pop_cases)), ")"))
	}
}


dt_data <- merge(dt_cases, dt_pop, by= c(group_by, var_age, "year"))

df_data <- as.data.frame(dt_data)
View(df_data)
str(df_data)