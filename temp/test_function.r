#DATA TABLE is DATA TABLE AT THE BEGINNING


#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
library(data.table)
library(ggplot2)


setwd("C:/Projects/Rcan/temp")
source("function.r")

## add function barchart

dat <- read.table(file="Exercise2-2.csv", header=TRUE,sep=",")
dat <- subset(dat, site=="Breast" & sex==2)

csu_asr(dat, var_age = "age_group", var_cases = "cases", var_py = "py",
                group_by = c("sex", "site"), missing_age = 19)

csu_asr(dat, var_age = "age_group", var_cases = "cases", var_py = "py",
                group_by = c("sex", "site", "year"), missing_age = 19)



subset(dasr, site=="Breast" & sex==2)
t <- subset(dat, site=="Breast" & sex==2)
tapply(t$cases, list(t$sex), sum)
tapply(t$py, list(t$sex), sum)



