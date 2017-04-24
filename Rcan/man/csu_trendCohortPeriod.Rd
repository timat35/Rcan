\name{csu_trendCohortPeriod}
\alias{csu_trendCohortPeriod}
\title{csu_trendCohortPeriod}

\description{
\code{csu_trendCohortPeriod}  plot cohort period age specific graph.
}


\usage{
csu_trend(df_data,
  df_data,
  var_age = "age",
  var_cases="cases",
  var_py="py",
  var_year = "year",
  type = "Cohort",
  year_group = 5,
  missing_age = NULL,
  db_rate = 100000,
  first_age = 6,
  last_age = 16,
  age_dropped=FALSE,
  logscale = TRUE,
  plot_title = "csu_title",
  format_export = NULL)
}
  
\arguments{
  \item{df_data}{
  Data (need to be R dataframe format, see example to import csv file).
	}
  \item{var_age}{
	(Optional, String) Age group variable: (numerical) 18 5-years age-group, 0-4, 5-9, .. 85+ and an optional missing age.\cr
	The value can be 1,2..19, as well as 0,5,..85 or other value as long are they are numerical.\cr
	Default is "age".
	}
   \item{var_cases}{
		(Optional, String) Number of cases variable (incidence).\cr
		Default is "cases".
	}
  \item{var_py}{
		(Optional, String) Population year variable.\cr
		Default is "py".
	}
  \item{var_year}{
		(Optional, String) Time variable (incidence).\cr
		Default is "year".
	}
  \item{type}{
		(Optional, String) "Cohort" to plot the cohort graph.\cr
		"Period" to plot the cohort graph.\cr
		"Both" to plot the cohort-period graph.\cr
		Default is "Cohort".
	}
  \item{year_group}{
		(Optional, numeric) Usually, data are regrouped in 5 years period. \cr.
		This can be change here. Default is 5.
   }
   \item{missing_age}{
		(Optional) Age value representing the missing age cases.
	}
  \item{db_rate}{
		(Optional) The denominator population. Default is 100000.
	}
  \item{first_age}{
		First age group included (for Truncated ASR). Default is 6 (25-39),  \bold{must} be between 1 and 17. 1 represents 0-4, 2 represents 5-9, ... 5 represente 20-24 etc.\cr
  }	
  \item{last_age}{
		Last age group included (for Truncated ASR). Default is 16 (75-79), \bold{must} be between 2 and 18. 2 represents 5-9, ... 5 represente 20-24, ... 18 represents 85+ etc.\cr
		To plot the age group from 15-19 years until 65-70 years, use: first_age(4) last_age(14)
  }
  \item{age_dropped}{
		(Optional) Only for truncated ASR. Option to add if the age group not used are already dropped from the data.\cr
		First_age and last_age option \bold{must} still be filled.
  }
  \item{logscale}{
		(Optional, Boolean) Y-axis scale (log or normal). Default is TRUE.
	}
  \item{plot_title}{
		(Optional) Title of the plot. Default is csu_title.
	}
  \item{format_export}{
		(Optional) export format 4 option: "pdf", "png", "tiff", "svg". The filename is the \code{plot_title} option.
	}

}

\details{
	This function is design the plot a the age-specific cohort and period plot. 
	The \code{type} option allow to choose between the 3 different graphics: "Cohort", "Period", or "Both".
	Please note than the cohort plot and the period plot can be superimposed if the \code{first_age} is too low.

}
\value{
	Return a plot.
	
}

\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_trend}}
}

\examples{

	data(csu_registry_data_1)
	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population 
	test <- csu_registry_data_2[csu_registry_data_2$registry == 84020,]
	test <- test[test$sex==1,]

	
	# plot cohort graph.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males")
						
	# plot cohort graph.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Period")
						
	# plot cohort graph.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both")
				
}