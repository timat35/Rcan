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


data(csu_registry_data_1)
data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# to select only 1 population.
test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

# plot age specific rate for 1 population.
csu_ageSpecific(test,
        plot_title = "Colombia, Liver, male",
        missing_age = 19
        )

# plot age specific rate for 1 population, and comparison with CI5X data.
csu_ageSpecific(test,
        plot_title = "Colombia, Liver, male",
        CI5_comparison = "Liver")

# plot age specific rate for 4 population, 
# legend at the bottom and comparison with CI5X data.
csu_ageSpecific(
  csu_registry_data_1,
  group_by="registry_label",
  legend=csu_trend_legend(position="bottom", nrow = 1),
  plot_title = "Liver, male",
  CI5_comparison = 7
  )





dat <- read.table(file="Exercise2-2.csv", header=TRUE,sep=",")
d <- subset(dat, site == "Stomach" & sex == 1 & year == 2008)


rate <- csu_ageSpecific(d, var_age = "age_group", var_cases = "cases", var_py = "py",
                        group_by = "site", missing_age = 19, plot_title = "My registry: 2008-2012",
                        CI5_comparison = "Stomach")

rate <-as.data.table(rate)
rate[nb_age_group == 16 & CSU_age_factor >= 16 , cases:=sum(cases), by="site"] ##add total_know


subset(dat, site=="Stomach" & sex == 1 & year == 2008)


test <-  17 / 17025*100000
test <-  (17+15+11) / 17025 * 100000

#

data("data_individual_file")
d <- csu_group_cases(data_individual_file, var_age = "age", group_by = c("sex"), var_year = "doi")
p <-  read.csv("Data_population-wide.csv")
dp <- csu_merge_cases_pop(d, p, var_age = "age_group")

