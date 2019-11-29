#DATA TABLE is DATA TABLE AT THE BEGINNING


# #update package (dev version)
# detach(package:Rcan)
# remove.packages("Rcan")
# devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


#update package (local version)
detach(package:Rcan)
remove.packages("Rcan")
install.packages("C:/Projects/Rcan/Rcan", repos = NULL, type = "source")

setwd("C:/Projects/Rcan/temp")
library(Rcan)
library(data.table)


db <- read.csv("check_ASR.csv")
db$age <- NULL
table(db$age_label)

csu_asr(db,"age_label","cases","py",var_age_group = c("country_label"), correction_info = TRUE,
        group_by = c("country_code","country_label","year","cancer_code","cancer_label","sex")
)

csu_asr(db,"age_label","cases","py",var_age_group = c("country_label"), correction_info = TRUE,
        group_by = c("country_code","country_label","year","cancer_code","cancer_label","sex"),
        missing_age = "NSP")


db <- read.csv("check_ASR.csv")
db$age_label <- NULL
table(db$age)

csu_asr(db,"age","cases","py",var_age_group = c("country_label"), correction_info = TRUE,
        group_by = c("country_code","country_label","year","cancer_code","cancer_label","sex")
)


csu_asr(db,"age","cases","py",var_age_group = c("country_label"), correction_info = TRUE,
        group_by = c("country_code","country_label","year","cancer_code","cancer_label","sex"),
        missing_age = 99)


csu_asr(db,"age","cases","py",var_age_group = c("country_label"), correction_info = TRUE,
        group_by = c("country_code","country_label","year","cancer_code","cancer_label","sex"),
        missing_age = 19)

db <- read.csv("check_ASR.csv")
db <- subset(db, select = c(cancer_label, age_label, cases, py))
csu_cumrisk(db, "age_label", "cases", "py", group_by = "cancer_label")

csu_cumrisk(db, "age_label", "cases", "py", missing_age = "Unknow", group_by = "cancer_label")