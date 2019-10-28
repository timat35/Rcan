#DATA TABLE is DATA TABLE AT THE BEGINNING


detach(package:Rcan)
remove.packages("Rcan")


#devtools::install_github("timat35/Rcan", subdir="Rcan")

install.packages("Rcan")

setwd("C:/Projects/Rcan/temp")


library(Rcan)

dat <- read.table(file="Exercise2.csv", header=TRUE,sep=",")

range(dat$age)
range(dat$age[dat$age != 999]) # without the '999' unknown age code

addmargins(table(dat$age))

d1 <- csu_group_cases(dat, var_age = "age", group_by = c("regcode", "reglabel"))

tail(d1)






data(ICD_group_GLOBOCAN)
data(data_individual_file)

#group individual data by 
# 5 year age group 

data_individual_file$py = 32 


df_data_age <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel", "site")) 

#group individual data by 
# 5 year age group 
# ICD grouping from dataframe ICD_group_GLOBOCAN

df_data_icd <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site") 

#group individual data by 
# 5 year age group 
# ICD grouping from dataframe ICD_group_GLOBOCAN
# year (extract from date of incidence)

df_data_year <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site",
  var_year = "doi")       
  

# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
                

inc_file <-ls_args$inc
pop_file <-ls_args$pop
group_by <- c("ICCC",  "YEAR", "SEX")
var_cases <- "CASES"
var_age <- "AGE_GROUP"
var_age_label <- "AGE_GROUP_LABEL"
var_pop <- "COUNT"
var_ref_count <- "REFERENCE_COUNT"


csu_merge_iccc_pop <- function(inc_file,
                              pop_file,
                              var_cases = "CASES",
                              var_age = "AGE_GROUP",
                              var_age_label = "AGE_GROUP_LABEL",
                              var_pop = "COUNT",
                              var_ref_count = "REFERENCE_COUNT",
                              group_by = NULL){
  
  df_inc <- read.table(inc_file, header=TRUE, sep="\t")
  df_pop <- read.table(pop_file, header=TRUE, sep="\t")
  
  dt_inc <- data.table(df_inc)
  dt_pop <- data.table(df_pop)

  table(dt_inc$ICCC)
  
  setnames(dt_inc, var_cases, "CSU_C")

  dt_inc <- dt_inc[get(var_age) < 3]
  
  group_by <- intersect(group_by,colnames(dt_inc))
  
  dt_inc <- dt_inc[, c(var_age, group_by, "CSU_C"), with = FALSE]
  dt_inc <-  dt_inc[,list(CSU_C = sum(CSU_C)), by=eval(colnames(dt_inc)[!colnames(dt_inc) %in% c("CSU_C")])]
  
  cj_var <-colnames(dt_inc)[!colnames(dt_inc) %in% c("CSU_C")]
  dt_temp = dt_inc[, do.call(CJ, c(.SD, unique=TRUE)), .SDcols=cj_var]
  
  dt_inc <- merge(dt_temp, dt_inc,by=colnames(dt_temp), all.x=TRUE)[, CSU_C := ifelse(is.na(CSU_C),0, CSU_C )]
  
  dt_pop <- dt_pop[get(var_pop) != 0,]
  dt_pop[[var_ref_count]] <-  dt_pop[[var_ref_count]]*100
  
  dt_all <- merge(dt_inc, dt_pop,by=intersect(colnames(dt_inc),colnames(dt_pop)), all.x=TRUE)
  
  
  setnames(dt_all,var_age,"CSU_A")
  setnames(dt_all,var_pop,"CSU_P")
  
  
  dt_all[is.na(get(var_age_label)), CSU_A := max(CSU_A)]
  dt_all[, YEAR:=NULL]

  dt_all <-  dt_all[,list(CSU_C = sum(CSU_C), CSU_P = sum(CSU_P)), by=eval(colnames(dt_all)[!colnames(dt_all) %in% c("CSU_C", "CSU_P")])]
  dt_all[, ICCC:=gsub("[a-z]", "", ICCC)]
  dt_all <-  dt_all[,list(CSU_C = sum(CSU_C)), by=eval(colnames(dt_all)[!colnames(dt_all) %in% c("CSU_C")])]

  #dt_sum <- dt_all 
  #dt_sum <-  dt_sum[,list(CSU_C = sum(CSU_C)), by=eval(colnames(dt_sum)[!colnames(dt_sum) %in% c("CSU_C")])]
  #dt_all <- rbind(dt_sum, dt_all)

  iccc_code <- as.data.table(read.csv(paste(sep="/", script.basename, "ICCC.csv")))
  iccc_code[, ICCC:=as.character(ICCC)]
  dt_all <- merge(dt_all,iccc_code, by=c("ICCC"), all=TRUE)

  dt_all[, ICCC:=NULL]
  dt_all <-  dt_all[,list(CSU_C = sum(CSU_C)), by=eval(colnames(dt_all)[!colnames(dt_all) %in% c("CSU_C")])]

  dt_all[is.na(ICCC_order),ICCC_order:=12]  
  dt_all[ICCC_order==12,ICCC_label:="Unknown"]
  dt_all[ICCC_order==12,ICCC_code:=" "]
  dt_all <- dt_all[!is.na(CSU_A),]


 
  setnames(dt_all,"CSU_P",var_pop)
  setnames(dt_all,"CSU_A",var_age)
  setnames(dt_all,"CSU_C",var_cases)
  return(dt_all)

}
