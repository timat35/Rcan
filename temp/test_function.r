#DATA TABLE is DATA TABLE AT THE BEGINNING


#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
library(data.table)
library(ggplot2)


setwd("C:/Projects/Rcan/temp")
source("function.r")

dat <- read.table(file="Exercise2-2.csv", header=TRUE,sep=",")
d <- subset(dat, site == "Stomach" & sex == 1 & year == 2008)


rate <- csu_ageSpecific(d, var_age = "age_group", var_cases = "cases", var_py = "py",
                        group_by = "site", missing_age = 19, plot_title = "My registry: 2008-2012")

rate <-as.data.table(rate)
rate[nb_age_group == 16 & CSU_age_factor >= 16 , cases:=sum(cases), by="site"] ##add total_know


subset(dat, site=="Stomach" & sex == 1 & year == 2008)


test <-  17 / 17025*100000
test <-  (17+15+11) / 17025 * 100000



## add function barchart


