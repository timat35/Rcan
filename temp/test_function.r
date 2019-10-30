#DATA TABLE is DATA TABLE AT THE BEGINNING


# #update package (dev version)
# detach(package:Rcan)
# remove.packages("Rcan")
# devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


#update package (local version)
# detach(package:Rcan)
remove.packages("Rcan")
install.packages("C:/Projects/Rcan/Rcan", repos = NULL, type = "source")

setwd("C:/Projects/Rcan/temp")

library(Rcan)

# READ THE DATA FILE
dat3 <- read.table(file="Exercise3-13.txt", header=TRUE,sep="\t")

# LOOK AT THE DATASET
str(dat3)
head(dat3)

## AGE EXPRESSED AS "00-04", "05-09", "10-14", ...
table(dat3$age_label)

### COMPUTE THE CUMULATIVE RISK 
csu_cumrisk(dat3, "age_label", "cases", "py",
                      group_by = c("registry" ), missing_age = 99)

# GENERATE AN AGE VARIABLE CODED 1, 2, 3, etc...
dat3$agecl <- c(factor(dat3$age_label, levels = c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",   
                                                  "50-54","55-59","60-64","65-69","70-74","75-79","80+")))

head(dat3)

### COMPUTE THE CUMULATIVE RISK 
csu_cumrisk(dat3, "agecl", "cases", "py", group_by = c("registry"),last_age = 12)


