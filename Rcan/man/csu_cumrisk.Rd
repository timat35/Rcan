\name{csu_cumrisk}
\alias{csu_cumrisk}
\title{csu_cumrisk}

\description{
\code{csu_cumrisk} Calculate cumulative risk across different population (Registry, year, sex...)
}
\usage{
csu_cumrisk(df_data,
		var_age = "age",
		var_cases = "cases",
		var_py ="py",
		group_by=NULL,
		missing_age = NULL,
		last_age = 15,
		var_st_err=NULL,
		correction_info=FALSE,
		var_cumrisk="cumrisk") 
}

\arguments{
	\item{df_data}{
  		Data (need to be R \code{data.frame} format, see example to import csv file).
	}
	\item{var_age}{
		Age group variable: (numerical) 18 5-years age-group, 0-4, 5-9, .. 85+ and an optional missing age.\cr
		The value can be 1,2..19, as well as 0,5,..85 or other value as long are they are numerical.
	}
	\item{var_cases}{
		Number of event (cases, deaths, ...) variable.
	}
	\item{var_py}{
		Population year variable.
	}
	\item{group_by}{
		A vector of variables to compare different ASR (sex, country, cancer ...).\cr
	}
	\item{missing_age}{
		Age value representing the missing age cases.\cr 
		If not precise, missing age value will not affect the calculation.
	}
	\item{last_age}{
		Last age group included, \bold{must} be between 2 and 17. 2 represents 5-9, ... 5 represents 20-24, ..., 17 represents 80-84, 18 represents 85+ etc.\cr
		default is 15 (calculates cumulative risk 0-74)
	}
	\item{var_st_err}{
		Calculate the Standard error and name of the new variable. 
	}
	\item{correction_info}{
		Logical value. if \code{TRUE} compute the variable "correction" : percentage of missing age cases.
	}
	\item{var_cumrisk}{
		Name of the new variable for the cumulative risk.
	}

}
\details{
	Calculate cumulative risk across different population\cr

}
\value{
	Return a \code{data.frame}.
	
}
\references{
	\url{https://publications.iarc.fr/_publications/media/download/3753/609d0d7711047dd76d7f3dbaa25d7f041fcd013e.pdf}

}
\author{
	Mathieu Laversanne
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
data(csu_registry_data_1)
data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# Cumulative risk, 0-74 years, with no missing age cases.
result <- csu_cumrisk(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" )
				  
# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  
# Cumulative risk, 0-74 years, with the percentage of correction due to missing age cases. 
result <- csu_cumrisk(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
				  missing_age = 19,				  
				  correction_info = TRUE)	
				  
# Cumulative risk 0-74 years and standard error with missing age.
result <- csu_cumrisk(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_st_err = "st_err",
				  missing_age = 99)
				  
# Cumulative risk, 0-69 years.
result <- csu_cumrisk(csu_registry_data_2, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
                  var_st_err = "st_err",
				  last_age = 14,
				  missing_age = 99)	
		    		  
}




