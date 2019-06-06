#DATA TABLE is DATA TABLE AT THE BEGINNING


#detach(package:Rcan)
#remove.packages("Rcan")
#devtools::install_github("timat35/Rcan", ref = "dev", subdir="Rcan")


library(Rcan)
library(data.table)
library(ggplot2)


setwd("C:/Projects/Rcan/temp")

dsr <- read.csv("test.csv")
csu_bar_top(dsr, var_value = "asr", var_bar = "site",
            group_by = "sex", nb_top = 15,
            plot_title = "Top 15 cancer sites",
            xtitle = "Age-standardised rate per 100,000", 
            color = c("#2c7bb6","#b62ca1"), 
            label_by = c("Males", "Females"), digits = 1)

Aucun souci.
Mais si on remplace “asr” par “cases“
csu_bar_top(dsr, var_value = "cases", var_bar = "site",
            group_by = "sex", nb_top = 15,
            plot_title = "Top 15 cancer sites",
            xtitle = "Number of cases", 
            color = c("#2c7bb6","#b62ca1"), 
            label_by = c("Males", "Females"), digits = 0)



