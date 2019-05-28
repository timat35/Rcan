#DATA TABLE is DATA TABLE AT THE BEGINNING


detach(package:Rcan)
remove.packages("Rcan")
devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
#check new function #

data("ICD_group_GLOBOCAN")
data("data_individual_file")
data("data_population_file")

df_data_year <- csu_group_cases(data_individual_file,
                                var_age="age",
                                group_by=c("sex", "regcode"),
                                df_ICD = ICD_group_GLOBOCAN,
                                var_ICD  ="site",
                                var_year = "doi")

df_data <- csu_merge_cases_pop(df_data_year, 
                               data_population_file, 
                               var_age = "age_group",
                               var_cases = "cases",
                               var_py = "pop")

head(data_population_file)



var_data_pop <- colnames(data_population_file)
    warning(paste0('The population dataset variable: ',var_data_pop,' is not present in the group_by option.\nPopulation data might have been summed, please check carefully.\n\n'))


paste0('The population dataset variable:',var_data_pop,'is not present in the group_by option.\nPopulation data might have been summed, please check carefully')


setwd("C:/Projects/Rcan/temp")


data_individual_file <- read.csv("data-individual_test.csv")
save(data_individual_file, file="data_individual_file.rda")

data_population_file <- read.csv("200612_population.csv")

unique(data_population_file$year)

data_population_file <- data_population_file[data_population_file$year >= 2008,]
save(data_population_file, file="data_population_file.rda")

dpop <- read.csv("200612_population.csv")
names(data_population_file)[3] <- "age_group"
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


