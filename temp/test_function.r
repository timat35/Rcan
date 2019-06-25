#DATA TABLE is DATA TABLE AT THE BEGINNING


detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
library(data.table)


library(ggplot2)


library(data.table)
library(ggplot2)
library(dplyr)
library(Epi)
library(Rcan)
library(xlsx)
library(reshape)



d <- read.xlsx("1-EC data-Golestan province_2004-2016.xlsx", sheetName = "Sheet1", stringsAsFactors = F)
d$sex <- as.numeric(d$Gender)
d$sex[d$sex == 5] <- NA
with(d, table(Gender, sex, useNA = "ifany"))
addmargins(table(d$sex, useNA = "ifany"))

d <- subset(d, !is.na(sex))
d$cancer <- paste("C",substr(d$Topography,1,2), sep = "")

dg <- csu_group_cases(d, var_age = "AGE", var_year = "Incidence.Year", group_by = c("cancer", "sex"))

source("function.r")
dg <- core.csu_group_cases(d, var_age = "AGE", group_by = c("cancer", "sex"),var_cases = NULL,df_ICD = NULL,var_ICD=NULL,var_year = "Incidence.Year", all_cancer=FALSE)

sum(dg$cases)
tail(dg)
nrow(subset(d, sex==2 & AGE>=70 & AGE<=74 & Incidence.Year==2016))

dpop <- read.xlsx("2-Population-Golestan province_2004-2016.xlsx", sheetName = "Total", stringsAsFactors = F)
dpop <- dpop %>% dplyr::rename(agegr=NA., M2004=X2004, F2004=NA..2, T2004=NA..3,
               M2005=X2005, F2005=NA..4, T2005=NA..5,
               M2006=X2006, F2006=NA..7, T2006=NA..8,
               M2007=X2007, F2007=NA..10, T2007=NA..11,
               M2008=X2008, F2008=NA..13, T2008=NA..14,
               M2009=X2009, F2009=NA..16, T2009=NA..17,
               M2010=X2010, F2010=NA..19, T2010=NA..20,
               M2011=X2011, F2011=NA..22, T2011=NA..23,
               M2012=X2012, F2012=NA..25, T2012=NA..26,
               M2013=X2013, F2013=NA..28, T2013=NA..29,
               M2014=X2014, F2014=NA..31, T2014=NA..32,
               M2015=X2015, F2015=NA..34, T2015=NA..35,
               M2016=X2016, F2016=NA..37, T2016=NA..38) %>% select(-contains('NA..'))

#dpop$agegr[dpop$agegr=="5--9"] <- "5-9"
#dpop$agegr[dpop$agegr=="10--14"] <- "10-14"
#dpop$agegr[dpop$agegr==">85"] <- "85+"

dpop <- dplyr::rename(dpop,  age_group_label = agegr)

table(dpop$age_group_label)

dpop <- dpop[-1,]
dpop <- dpop[-19,]


dpopm <- reshape::melt(dpop, id="age_group_label", variable_name = "year")

dpopm$sex <- substr(dpopm$year,1,1)
dpopm$year <- as.numeric(substr(dpopm$year,2,5))
dpopm$sex <- ifelse(dpopm$sex=="M",1,ifelse(dpopm$sex=="F",2,0))

with(dpopm, table(sex, year))

dpopm$pop <- as.numeric(levels(dpopm$value))[dpopm$value]
dpopm$value <- NULL

dgp <- csu_merge_cases_pop(dg, dpopm, var_age = "age_group_label", var_cases = "cases", 
                    group_by = c("sex"), var_py = "pop")


setwd("C:/Projects/Rcan/temp")
load("oeso.RData")

names(dg)
names(dg)[3] <- "yy"

names(dpopm)
names(dpopm)[1] <- "yy"

df_data <- csu_merge_cases_pop(dg, dpopm, var_age = "age", var_py = "pop", group_by = c("sex", "yy"))
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