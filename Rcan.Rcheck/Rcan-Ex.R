pkgname <- "Rcan"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('Rcan')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Rcan-package")
### * Rcan-package

flush(stderr()); flush(stdout())

### Name: Rcan-package
### Title: R package to help managing and analysing cancer data
### Aliases: Rcan-package Rcan
### Keywords: package

### ** Examples

data(csu_registry_data_2)
				  
# ASR with standard error with missing age.
df_asr <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err")

df_asr[1:4,]	

# ASR with standard error with missing age.		  
readline(prompt="Press [enter] to continue to EAPC")

# EAPC with standard error		
	  
df_eapc <- csu_eapc(df_asr,
					"asr", "year",
					group_by=c("registry", "registry_label", "sex", "ethnic" ))
					
df_eapc[1:4,]	

# EAPC with standard error				  
readline(prompt="Press [enter] to continue to age specific graph")

data(csu_registry_data_1)

# plot age specific rate for 1 population.
df_colombia <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

csu_ageSpecific(df_colombia,
			plot_title = "Colombia, Liver, male")
				
# plot age specific rate for 1 population, and comparison with CI5X data.
csu_ageSpecific(df_colombia,
				plot_title = "Colombia, Liver, male",
				CI5_comparison = "Liver")
				
# plot age specific rate for 4 population, legend at the bottom and comparison with CI5X data.
csu_ageSpecific(csu_registry_data_1,
				group_by="registry_label",
				legend=csu_trend_legend(position="bottom", nrow = 2),
				plot_title = "Liver, male",
				CI5_comparison = 7)
				



cleanEx()
nameEx("csu_ageSpecific")
### * csu_ageSpecific

flush(stderr()); flush(stdout())

### Name: csu_ageSpecific
### Title: csu_ageSpecific
### Aliases: csu_ageSpecific

### ** Examples


	data(csu_registry_data_1)
	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population.
	test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

	# plot age specific rate for 1 population.
	csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male")
				
	# plot age specific rate for 1 population, and comparison with CI5X data.
	csu_ageSpecific(test,
					plot_title = "Colombia, Liver, male",
					CI5_comparison = "Liver")

	# plot age specific rate for 4 population, legend at the bottom and comparison with CI5X data.
	csu_ageSpecific(csu_registry_data_1,
					group_by="registry_label",
					legend=csu_trend_legend(position="bottom", nrow = 2),
					plot_title = "Liver, male",
					CI5_comparison = 7)
	
	# plot age specific rate for 4 population, legend at the right.
	csu_ageSpecific(csu_registry_data_1,
					group_by="registry_label",
					legend=csu_trend_legend(position="right", right_space_margin = 6.5),
					plot_title = "Liver, male")	

    # Plot embedded in a graphic device
	pdf("test.pdf")
	csu_ageSpecific(csu_registry_data_1,
					group_by="registry_label",
					legend=csu_trend_legend(position="right", right_space_margin = 6.5),
					plot_title = "Liver, male",
					graph_dev=TRUE)	
					
	csu_ageSpecific(csu_registry_data_1,
				group_by="registry_label",
				legend=csu_trend_legend(position="bottom", nrow = 2),
				plot_title = "Liver, male",
				CI5_comparison = 7, 
				graph_dev=FALSE)
				
	dev.off()
	

	
	
	




	



cleanEx()
nameEx("csu_asr")
### * csu_asr

flush(stderr()); flush(stdout())

### Name: csu_asr
### Title: csu_asr
### Aliases: csu_asr

### ** Examples

data(csu_registry_data_1)
data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# Age standardized rate (ASR) with no missing age cases.
result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))
				  
# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  
# ASR,  with the percentage of correction due to missing age cases. 
result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"),
				  missing_age = 19,				  
				  correction_info = TRUE)	
				  
# ASR and standard error with missing age.
result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
				  missing_age = 99)
				  
# Truncated ASR, 25-69 years.
result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
				  first_age = 6, 
				  last_age = 14,
				  missing_age = 99)	

# Truncated ASR, 0-15 with denominator population = 1000000. 
result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err",
				  first_age = 1, 
				  last_age = 3, 
				  missing_age = 99,	
				  db_rate = 1000000)		
				  
# ASR with EURO population as reference (instead of SEGI)
result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"),
				  missing_age = 19,
                  pop_base = "EURO")
				    		  



cleanEx()
nameEx("csu_ci5x_mean")
### * csu_ci5x_mean

flush(stderr()); flush(stdout())

### Name: csu_ci5x_mean
### Title: cancer registry data
### Aliases: csu_ci5x_mean
### Keywords: datasets

### ** Examples


data(csu_registry_data_1)
test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male",
                CI5_comparison = "Liver")
 
#See more examples here:
help(csu_ageSpecific)




cleanEx()
nameEx("csu_eapc")
### * csu_eapc

flush(stderr()); flush(stdout())

### Name: csu_eapc
### Title: csu_eapc
### Aliases: csu_eapc

### ** Examples


data(csu_registry_data_2)

# you import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# Estimated Annual Percentage Change (EAPC) base on ASR.


df_asr <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
				  missing_age = 99)
				  
result <- csu_eapc(df_asr,
					"asr", "year",
					group_by=c("registry", "registry_label", "sex", "ethnic" ))

				  
# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  		  



cleanEx()
nameEx("csu_registry_data_1")
### * csu_registry_data_1

flush(stderr()); flush(stdout())

### Name: csu_registry_data_1
### Title: cancer registry data
### Aliases: csu_registry_data_1
### Keywords: datasets

### ** Examples


data(csu_registry_data_1)

# Age standardized rate (ASR) with no missing age cases.
result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))
				  
#See more examples here:
help(csu_asr)

				  
				  



cleanEx()
nameEx("csu_registry_data_2")
### * csu_registry_data_2

flush(stderr()); flush(stdout())

### Name: csu_registry_data_2
### Title: cancer registry data
### Aliases: csu_registry_data_2
### Keywords: datasets

### ** Examples


data(csu_registry_data_2)
				  
# ASR and standard error with missing age.
result <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
				  missing_age = 99,
                  var_st_err = "st_err")
				  
#See more examples here:
help(csu_asr)	
				  



cleanEx()
nameEx("csu_trend")
### * csu_trend

flush(stderr()); flush(stdout())

### Name: csu_trend
### Title: csu_trend
### Aliases: csu_trend

### ** Examples


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
	csu_trend(df_asr, group_by="sex",
                plot_title = "Colombia, Liver")
				
	# plot ASR over year, by sex, with small smoothing.
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.3)
			  
	# plot ASR over year, by sex, with high smoothing.
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.5)
			  
	# Plot embedded in a graphic device
	pdf("example_test.pdf")
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.3,
			  graph_dev=TRUE)
			  
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.5)
				
	dev.off()
				



cleanEx()
nameEx("csu_trendCohortPeriod")
### * csu_trendCohortPeriod

flush(stderr()); flush(stdout())

### Name: csu_trendCohortPeriod
### Title: csu_trendCohortPeriod
### Aliases: csu_trendCohortPeriod

### ** Examples


	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population 
	test <- csu_registry_data_2[csu_registry_data_2$registry == 84020,]
	test <- test[test$sex==1,]

	
	# plot cohort graph from 25-29 years until 75-79 years.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males")
						
	# plot Period graph from 0-5 until 85+.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Period",
						first_age=1,
						last_age=18)
						
	# plot Cohort-Period graph from 30-64 years until 70-74 years.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15)

	# plot Cohort-Period graph from 30-64 years until 70-74 years with Y axis normal scale.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15,
						logscale=FALSE)
						
	# plot Cohort graph from 25-29 years until 75-79 years, with data grouped in 2 years period.
	csu_trendCohortPeriod(df_data=test,
						  missing_age =99,
						  plot_title = "USA, Liver, males",
						  type="Cohort",
						  year_group = 2)
						  
	# Plot embedded in a graphic device
	pdf("example_test.pdf")
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15,
						graph_dev=TRUE)
			  
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15,
						logscale=FALSE)
				
	dev.off()
				



cleanEx()
nameEx("csu_trend_legend")
### * csu_trend_legend

flush(stderr()); flush(stdout())

### Name: csu_trend_legend
### Title: csu_trend_legend
### Aliases: csu_trend_legend

### ** Examples


data(csu_registry_data_1)

csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(title="registry", position="bottom", nrow = 2),
                plot_title = "Legend: bottom")
				
csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(position="right", right_space_margin = 2),
                plot_title = "Legend: right, cut")

csu_ageSpecific(csu_registry_data_1,
                group_by="registry_label",
                legend=csu_trend_legend(position="right", right_space_margin = 6.5),
                plot_title = "Legend: right")
				
#See more examples here:
help(csu_ageSpecific)
	  		  



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
