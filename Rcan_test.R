
rcan_folder <- "c:/projects/Rcan"
#install.packages("http://timat.org/matR/Rcan.tar.gz", repos=NULL)
library(Rcan)
library(data.table)
library(ggplot2)
library(grid)


source(paste0(rcan_folder, "/Rcan/R/helper.r"))
source(paste0(rcan_folder, "/Rcan/R/csu_ageSpecific.r"))


#test package
# detach(package:Rcan)
# remove.packages("Rcan")
# install.packages("C:/Projects/Rcan/Rcan_1.3.0.tar.gz", repos=NULL)




# test ---------

data(csu_registry_data_1)
data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# to select only 1 population.
test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

# plot age specific rate for 1 population.
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male")


data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# to select only 1 population 
test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]

# to change sex variable to factor with label
test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))

# to calculate the asr
df_asr <- csu_asr(test,missing_age = 99,
                  group_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))

# plot ASR ove year, by sex.
csu_trend(df_asr, 
          plot_title = "Colombia, Liver, male", smoothing = NULL)


# test csu trend --------


mydata <-  read.csv("data_test/data_cervix.csv", sep=",")


# to change sex variable to factor with label
mydata$sex <- factor(mydata$sex, levels=c(1,2), labels=c("Male", "Female"))
mydata$type <- factor(mydata$type, levels=c(1,2), labels=c("Incidence", "Mortality"))

# to calculate the asr
df_asr <- csu_asr(mydata,missing_age = 19,
                  group_by  = c("country", "country_label", "year", "sex","type"),
                  var_age_group =  c("country", "country_label","type"))

df_Incidence <- df_asr[df_asr$type =="Incidence",]
df_mortality <- df_asr[df_asr$type =="Mortality",]
df_country <- df_asr[df_asr$country_label =="Colombia",]



csu_trend(df_Incidence, group_by="country_label",
          var_trend="asr",
          plot_title = "Cervix",
          smoothing = 0.5,)


csu_trend(df_mortality, group_by="country_label",
          plot_title = "Cervix",
          logscale = FALSE)

csu_trend(df_country, group_by="type",
          plot_title = "Cervix",
          legend = csu_trend_legend(position = "right", right_space_margin = 2))

df_unique1 <- mydata[mydata$type=="Incidence" & mydata$country_label == "Colombia",]
df_unique2 <- mydata[mydata$type=="Incidence" & mydata$country_label == "Costa Rica",]

csu_trendCohortPeriod(df_unique1,missing_age = 19, type="Period", plot_title = "Colombia, Cervix\n Incidence")
csu_trendCohortPeriod(df_unique2,missing_age = 19, type="Cohort",plot_title = "Costa rica, Cervix\n Incidence")


#--------------
mydata <-  read.csv("data_test/data_colorectum.csv", sep=",")


# to change sex variable to factor with label
mydata$sex <- factor(mydata$sex, levels=c(1,2), labels=c("Male", "Female"))
mydata$type <- factor(mydata$type, levels=c(1,2), labels=c("Incidence", "Mortality"))

# to calculate the asr
df_asr <- csu_asr(mydata,missing_age = 19,
                  group_by  = c("country", "country_label", "year", "sex","type"),
                  var_age_group =  c("country", "country_label","type"))

df_Incidence <- df_asr[df_asr$type =="Incidence"& df_asr$sex=="Male",]
df_mortality <- df_asr[df_asr$type =="Mortality"& df_asr$sex=="Male",]
df_country <- df_asr[df_asr$country_label =="Colombia",]



csu_trend(df_Incidence, group_by="country_label",
          var_trend="asr",
          plot_title = "Colorectum",
          smoothing = 0.5,
          legend = csu_trend_legend(position = "right", right_space_margin = 2))


csu_trend(df_mortality, group_by="country_label",
          plot_title = "Colorectum",
          logscale = TRUE)

csu_trend(df_country, group_by="type",
          plot_title = "Colorectum",
          legend = csu_trend_legend(position = "right", right_space_margin = 2))

df_unique1 <- mydata[mydata$type=="Incidence" & mydata$country_label == "Colombia" & mydata$sex=="Male",]
df_unique2 <- mydata[mydata$type=="Incidence" & mydata$country_label == "Costa Rica" & mydata$sex=="Male",]

csu_trendCohortPeriod(df_unique1,missing_age = 19, type="Both", plot_title = "Colombia, Colorectum\n Incidence, Male")
csu_trendCohortPeriod(df_unique2,missing_age = 19, type="Both", plot_title = "Costa rica, Colorectum\n Incidence, Male")
