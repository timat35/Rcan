\name{Rcan-package}
\alias{Rcan-package}
\alias{Rcan}
\docType{package}
\title{
\packageTitle{Rcan}
}
\description{
\packageDescription{Rcan}
}

\author{
\packageAuthor{Rcan}

Maintainer: \packageMaintainer{Rcan}
}
\references{
\url{https://publications.iarc.fr/_publications/media/download/3753/609d0d7711047dd76d7f3dbaa25d7f041fcd013e.pdf}
}

\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{csu_merge_cases_pop}}
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_ageSpecific_top}}
	\code{\link{csu_bar_top}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
}

\examples{


data(ICD_group_GLOBOCAN)
data(data_individual_file)


#group individual data by 
#	5 year age group 
#	ICD grouping from dataframe ICD_group_GLOBOCAN
df_data_year <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site",
  var_year = "doi")     

head(df_data_year)
# individual cases grouped by ICD and 5 years age group and year.
readline(prompt="Press [enter] to continue to merge cases and population)")	


data(data_population_file)	

df_data <- csu_merge_cases_pop(
	df_data_year, 
	data_population_file, 
	var_age = "age_group",
	var_cases = "cases",
	var_py = "pop",
	group_by = c("sex"))

head(df_data)
#Merge 5-years age grouped data with population by year (automatic) and sex
readline(prompt="Press [enter] to continue to merge cases and population)")	


data(csu_registry_data_2)
				  
# ASR with standard error with missing age.
df_asr <- csu_asr(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_age_group = c("registry_label"), 
                  var_st_err = "st_err")

df_asr[1:4,]	


\donttest{
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
	
}	
}

\keyword{ package }


