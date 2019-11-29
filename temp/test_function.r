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
        group_by = c("country_code","country_label","year","cancer_code","cancer_label","sex"))



"0-" %in% unique(db[['age_label']])