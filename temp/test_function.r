#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


#DATA TABLE is DATA TABLE AT THE BEGINNING


library(Rcan)
library(data.table)
library(ggplot2)
library(scales)

setwd("C:/Projects/Rcan/temp")
source("function.r")

#prepare data


dcas <- read.csv("Exercise2_qui.csv")
dpop <- read.csv("200612_population.csv")

names(dpop)[3] <- "age_group"

data("ICD_group_GLOBOCAN")

dcases <- csu_group_cases(dcas, var_age = "age", group_by = c("sex"), df_ICD = ICD_group_GLOBOCAN, var_ICD = "site", var_year = "doi", all_cancer=FALSE)
dcaspop <- csu_merge_cases_pop(dcases, dpop, var_age = "age_group", var_cases = "cases", var_py = "pop", group_by = c("sex"))

#calculate ASR
str(dcaspop)
table(dcaspop[,c("age_group")])

dcaspop <- as.data.table(dcaspop)



dcasasr <- csu_asr(dcaspop,
	"age_group", 
	"cases",
	"pop",
	group_by=c("sex", "ICD_group", "LABEL", "year"),
	missing_age =19)


dcasasr <- as.data.table(dcasasr)
dcasasr <- dcasasr[!LABEL %in% c("Other", "Other skin"), ]
dcasasr <- dcasasr[!LABEL %in% c("Other", "Other skin"), ]

#casasr[, sex:=factor(sex, levels=c(1,2), labels=c("Male", "Female"))] ## need factor
#ength(levels(dcasasr$sex))

source("function.r")
data <- csu_bar_top(
   dcasasr,
   var_top="cases",
   var_bar="LABEL",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   ytitle= "Number of cases",
   label_by=c("Male", "Female"),
   color = c("#2c7bb6","#b62ca1"),
   digits=1) 

dcasasr_male <- dcasasr[sex==1,]
dcasasr_male <- merge(dcasasr_male, color_test, by="LABEL")

source("function.r")
data <- csu_bar_top(
   dcasasr_male,
   var_top="cases",
   var_bar="LABEL",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   ytitle= "Number of cases",
   color="hex_color",
   digits=1) 

data <- csu_bar_top(
   dcasasr_male,
   var_top="cases",
   var_bar="LABEL",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   ytitle= "Number of cases",
   color= c("#2c7bb6","#b62ca1"),
   digits=1) 

## add function barchart


