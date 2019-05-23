#DATA TABLE is DATA TABLE AT THE BEGINNING


detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
#check new function #


setwd("C:/Projects/Rcan/temp")


dcas <- read.csv("Exercise2_qui.csv")
dpop <- read.csv("200612_population.csv")
names(dpop)[3] <- "age_group"
data("ICD_group_GLOBOCAN")

dcases <- csu_group_cases(dcas, var_age = "age", group_by = c("sex"), df_ICD = ICD_group_GLOBOCAN, var_ICD = "site", var_year = "doi", all_cancer=FALSE)
dcaspop <- csu_merge_cases_pop(dcases, dpop, var_age = "age_group", var_cases = "cases", var_py = "pop", group_by = c("sex"))


dcasasr <- csu_asr(dcaspop,
	"age_group", 
	"cases",
	"pop",
	group_by=c("sex", "ICD_group", "LABEL"),
	missing_age =19)


dcasasr <- dcasasr[!dcasasr$LABEL %in% c("Other", "Other skin"), ]

#casasr[, sex:=factor(sex, levels=c(1,2), labels=c("Male", "Female"))] ## need factor
#ength(levels(dcasasr$sex))

dcasasr_male <- dcasasr[dcasasr$sex==1,]


data <- csu_bar_top(
   dcasasr_male,
   var_value="cases",
   var_bar="LABEL",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   xtitle= "Number of cases",
   color= c("#2c7bb6"),
   digits=0) 

color_test <- read.csv("temp_color.csv")
dcasasr_male <- merge(dcasasr_male, color_test, by="LABEL")

data <- csu_bar_top(
   dcasasr_male,
   var_value="cases",
   var_bar="LABEL",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   xtitle= "Number of cases",
   color="hex_color",
   digits=0) 



data2 <- csu_bar_top(
   dcasasr,
   var_value="cases",
   var_bar="LABEL",
   group_by="sex",
   nb_top = 15,
   plot_title = "Top 15 cancer sites",
   xtitle= "Number of cases",
   label_by=c("Male", "Female"),
   color = c("#2c7bb6","#b62ca1"),
   digits=0) 

data2 <- csu_bar_top(
   dcasasr,
   var_value="asr",
   var_bar="LABEL",
   group_by="sex",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   xtitle= "Age-standardized rate per 100,00",
   label_by=c("Male", "Female"),
   color = c("#2c7bb6","#b62ca1"),
   digits=1) 



## add function barchart


