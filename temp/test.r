
# group ICD
library(data.table)
setwd("c:/projects/Rcan/temp")
source("function.r")



inc_file <- "cases1.csv"
ICD_file <- "Datos-icd.csv"
var_ICD <- "I10"
var_cases <- NULL
var_age <- "AGE"
var_year <- NULL
column_group_list <- NULL

group_by <- c("SEX")

df_inc <- read.table(inc_file, header = TRUE, sep = ",")
dt_inc <- data.table(df_inc)

if (is.null(var_cases)) {
	var_cases <- "cases"
	dt_inc[, cases:= 1]
} else {
	setnames(dt_inc, var_cases, "cases")
}

dt_inc <- dt_inc[, c(var_cases,var_age,var_ICD,group_by), with = FALSE]

# merge with ICD 
f_ICD <- read.table(ICD_file, header=TRUE, sep=",")
dt_ICD <- data.table(df_ICD)
setkeyv(dt_ICD,c("LABEL", "ICD")) 
dt_ICD[, ICD_group:= lapply(LABEL, function(x) {icd_group(as.vector(dt_ICD[LABEL == x, ]$ICD))})]
list_ICD <- dt_ICD$ICD

dt_inc[, temp := as.character(get(var_ICD))]
dt_inc[, ICD := list_ICD[match(dt_inc$temp, list_ICD)]]
dt_inc[!is.na(ICD), temp := NA]
dt_inc[, temp:= substr(temp, 1, 3)] 
dt_inc[, ICD:=list_ICD[match(dt_inc$temp, list_ICD)]]
dt_inc <- dt_inc[!is.na(ICD), ]
dt_inc[,c("temp", var_ICD) := list(NULL, NULL)]
dt_inc <- merge(dt_inc, dt_ICD, by=c("ICD"))
group_by <- c(group_by, colnames(dt_ICD))

column_group_list <- list(colnames(dt_ICD))
column_group_list[[1]]  <- intersect(column_group_list[[1]], colnames(dt_inc))

#  create age group 
dt_inc[, age_group:= cut(get(var_age), c(seq(0, 85, 5), 150), include.lowest = TRUE, right=FALSE)]
dt_inc[, age_group_label := as.character(age_group)]
dt_inc[, temp1 := sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\1",age_group_label)]
dt_inc[, temp2 := as.numeric(sub("\\[(\\d{1,3}),(\\d{1,3}).+", "\\2",age_group_label))]
dt_inc[, age_group :=  ifelse(temp2 == 150 ,18,temp2/5)]
dt_inc[, age_group_label := ifelse(temp2 == 150, paste0(temp1,"+"), paste0(temp1,"-", as.character(temp2-1)))] 
dt_inc[is.na(age_group), age_group :=  19]
dt_inc[,c("temp1","temp2", var_age) := list(NULL, NULL, NULL)]

dt_inc <-  dt_inc[,list(cases = sum(cases)),by=eval(colnames(dt_inc)[!colnames(dt_inc) %in% c("cases")])]

column_group_list <- c(column_group_list, list(c("age_group", "age_group_label")))

if (!is.null(column_group_list)){
	cj_var <- colnames(dt_inc)[!colnames(dt_inc) %in% unlist(lapply(column_group_list, `[`, -1))]
} 

cj_var <-cj_var[!cj_var %in% c("cases")]

dt_temp = dt_inc[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols=cj_var]


dt_temp[1:20]

##keep ICD group label
if (!is.null(column_group_list)){
	nb_group <- length(column_group_list)
	for( i in 1:nb_group) {
	  dt_col_group <- unique(dt_inc[, column_group_list[[i]], with=FALSE])
	  dt_temp <- merge(dt_temp, dt_col_group,by= column_group_list[[i]][[1]], all.x=TRUE)
	}
}

dt_inc <- merge(dt_temp, dt_inc,by=colnames(dt_temp), all.x=TRUE)[, cases := ifelse(is.na(cases),0, cases )]
dt_inc[1:20]