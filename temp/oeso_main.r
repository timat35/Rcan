setwd("C:/Projects/Rcan/temp")

install.packages("dplyr")
install.packages("reshape")

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
sum(dg$cases)
head(dg)
subset(d, sex==1 & AGE>=40 & AGE<=44 & Incidence.Year==2004)

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

nrow(merge(dg, dgp, by=c("age_group", "cancer", "sex", "year", "age_group_label", "cases")))
nrow(merge(dg, dgp, by=c("age_group", "cancer", "sex", "year", "cases")))
head(dgp)
tail(dgp)

dpopc <- cast(dpopm, age_group_label+sex ~ year, value = "pop")

dgp <- csu_merge_cases_pop(dg, dpopc, var_age = "age_group_label", var_cases = "cases", 
                           group_by = c("sex"), var_py = NULL)

nrow(merge(dg, dgp, by=c("age_group", "cancer", "sex", "year", "age_group_label", "cases")))
nrow(merge(dg, dgp, by=c("age_group", "cancer", "sex", "year", "cases")))
head(dgp)

