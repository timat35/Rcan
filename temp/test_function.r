#DATA TABLE is DATA TABLE AT THE BEGINNING


#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
library(data.table)
library(ggplot2)


setwd("C:/Projects/Rcan/temp")


dat <- read.csv(file="Exercise5.csv", header=TRUE)

dat$year <-as.character(dat$year)


csu_ageSpecific_top(dat, var_age = "age_group", var_cases = "cases", var_py = "py", nb_top = 5, 
                    var_top = "site", group_by = "sex", missing_age = 19, plot_title = "My registry: 2008-2012")


csu_ageSpecific(dat,
            var_age="age_group",
        var_cases="cases",
        var_py="py",
        group_by = "year", 
        missing_age = 19,
      ) 
help(csu_ageSpecific_top)










dasr <- csu_asr(dat, var_age = "age_group", var_cases = "cases", var_py = "py",
                group_by = c("sex", "site"), missing_age = 19)
dasr <- subset(dasr, site != "Other" & site != "Other skin")

dasr <- dasr[order(dasr$sex, -dasr$asr),]

par(mfrow=c(1,2))

csu_bar_top(dasr[dasr$sex==1,],var_value = "asr", var_bar = "site", color="lightblue")

csu_bar_top(dasr[dasr$sex==2,],var_value = "asr", var_bar = "site", color="tomato")

csu_bar_top(dasr,var_value = "asr", var_bar = "site",group_by="sex",nb_top=15, 
      color=c("lightblue", "tomato"),label_by=c("Males","Females"),
      xtitle="ASR per 100,000")

barplot(rev(dasr$asr[dasr$sex==1]), horiz = TRUE, col = "tomato", xlab = "ASR per 100,000",
        names.arg = rev(dasr$site[dasr$sex==1]), las=2, main = "Males")
barplot(rev(dasr$asr[dasr$sex==2]), horiz = TRUE, col = "lightblue", xlab = "ASR per 100,000",
        names.arg = rev(dasr$site[dasr$sex==2]), las=2, main = "Females")



