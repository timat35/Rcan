#DATA TABLE is DATA TABLE AT THE BEGINNING


detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
library(data.table)
library(ggplot2)


setwd("C:/Projects/Rcan/temp")
load("oeso.RData")



df_data <- csu_merge_cases_pop(dg, dpopm, var_age = "age", var_py = "pop", group_by = c("sex", "year"))
csu_asr (df_data ,  "age", "cases", "pop", group_by=c("sex", "year"),first_age=4, last_age = 18, age_dropped=TRUE)

df_test <- subset(df_data, sex==1 & year == 2004)
csu_asr (df_test ,  "age", "cases", "pop",first_age=4, last_age = 18, age_dropped=TRUE)
write.csv(df_test, "test.csv")



dat <- read.table(file="Exercise5.csv", header=TRUE,sep=",")

dat$year <- as.character(dat$year)

csu_ageSpecific_top(dat, var_age = "age_group", var_cases = "cases", var_py = "py", nb_top = 5, 
                    var_top = "site", group_by = "year", missing_age = 19, plot_title = "My registry: 2008-2012")




dg <- read.csv("Exercise2_izm_aggreg.csv")

dpop <- read.table("Data-population.txt", header = T, sep = "\t")
names(dpop)[2] <- "age_group"
names(dpop)[3] <- "YEAR"

dgp <- csu_merge_cases_pop(dg, dpop, var_age = "age_group", var_cases = "cases", 
                           var_py = "POP", group_by = c("SEX"))

data(ICD_group_GLOBOCAN)
data(data_individual_file)

names(data_individual_file)[5] <- "year"

df_data_year <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site",
  var_year = "year")   


data(csu_registry_data_1)
data(csu_registry_data_2)



result1 <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))

result2 <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ))


dt_test <- subset(csu_registry_data_1, registry %in% c("3604", "35604"))
result1 <- csu_asr(dt_test, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))

result2 <- csu_asr(dt_test, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ))

library(Rcan)

core.csu_icd_ungroup <- function(icd_group) {

  icd_group <- gsub("\\s", "", icd_group)
  
  icd_list <- NULL
  ICD_reg <-"([A-Za-z]\\d+)(\\W?)(.+)?"

  while (nchar(icd_group)>=3) {

    icd_start <- sub(ICD_reg, "\\1", icd_group)
    icd_mark <- sub(ICD_reg, "\\2", icd_group)
    icd_group <- sub(ICD_reg, "\\3", icd_group)


    if (icd_mark == "-") {

      letter_start <- sub("([A-Za-z])(\\d+)", "\\1", icd_start)
      code_start <- sub("([A-Za-z])(\\d+)", "\\2", icd_start)
      code_nchar <- nchar(code_start)
      code_start <- as.numeric(code_start)
      code_end <- as.numeric(sub("[A-Za-z]?(\\d+)(.+)?", "\\1", icd_group))

      for (code in code_start:code_end) {
        icd_list <- c(icd_list, paste0(letter_start, sprintf(paste0("%0",code_nchar,"d"), code)))
      }
      icd_group <- sub("([A-Za-z]?\\d+)(\\W?)(.+)?", "\\3", icd_group) 
    }
    else  {
      icd_list <- c(icd_list, sub("([A-Za-z]\\d+)", "\\1", icd_start))
    }
  }

  return(icd_list)
}



test <- "C18,C19,C21, D09-14, D15"
list <- core.csu_icd_ungroup(test)
Rcan:::core.csu_icd_group(list)