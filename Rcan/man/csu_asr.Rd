\name{csu_asr}
\alias{csu_asr}
\title{csu_asr}

\description{
\code{csu_asr} calculate Age-Standardized Rate (ASR) and Truncated Age-Standardized Rate (TASR) across different population (Registry, year, sex...)
}
\usage{
csu_asr(df_data,
		var_age = "age",
		var_cases = "cases",
		var_py ="py",
		group_by=NULL,
		var_age_group=NULL,
		missing_age = NULL,
		db_rate = 100000,
		first_age = 1,
		last_age = 18,
		pop_base = "SEGI",
		crude_rate = FALSE,
		var_st_err = NULL,
		correction_info = FALSE,
		var_asr = "asr",
		age_dropped = FALSE)
}

\arguments{
  \item{df_data}{
  Data (need to be R \code{data.frame} format, see example to import csv file).
	}
	\item{var_age}{
		Age variable. Several format are accepted 
		\tabular{lll}{
			1 \tab "0-4" \tab 0 \cr
			2 \tab "5-9" \tab 5 \cr
			3 \tab "10-14" \tab 10 \cr
			... \tab ... \tab ... \cr
			17 \tab "80-84" \tab 80 \cr
			18 \tab "85+" \tab 85 \cr
		}
		Missing age value must be precise in the option \code{missing_age}.\cr
		Last age group will always be considere without size (ie: 80+,85+, etc..). 
	}
  \item{var_cases}{
	Number of event (cases, deaths, ...) variable.
	}
  \item{var_py}{
	Population year variable.
	}
  \item{group_by}{
		A vector of variables to compare different ASR (sex, country, cancer ...).\cr
		Any variable included in the next option var_age_group \bold{must} be also include here.
}
  \item{var_age_group}{
		Variables over which the number of population age-group might change.\cr
		For example the last age group (70+, 75+, 80+ or 85+) can vary across registries, or year, but not across sex.
}
  \item{missing_age}{
		Age value representing the missing age cases.\cr 
		If not precise, missing age value will not affect the calculation.
}
  \item{db_rate}{
		The denominator population. Default is 100000.
}

  \item{first_age}{
		First age group included (for Truncated ASR), \bold{must} be between 1 and 17. 1 represents 0-4, 2 represents 5-9, ... 5 represents 20-24 etc.\cr
}
  \item{last_age}{
		Last age group included (for Truncated ASR), \bold{must} be between 2 and 18. 2 represents 5-9, ... 5 represents 20-24, ... 18 represents 85+ etc.\cr
		To calculate ASR for 15-69 years, use: \code{first_age(4), last_age(14)}.
}
  \item{pop_base}{
   Select the standard population:\cr
  	\tabular{ll}{
		"SEGI" \tab SEGI world standard population.\cr
		"EURO" \tab EURO is the European standard population.\cr
	}
}
	\item{crude_rate}{
		Logical value. if \code{TRUE} calculate the crude rate instead of the ASR.\cr
		Variables \code{var_age_group}, \code{missing_age}, \code{pop_base}, \code{var_st_err}, \code{correction_info} are not used.\cr
		Default name of the new variable is "crude_rate".
}

  \item{var_st_err}{
		Calculate the Standard error and name of the new variable. 
}
  \item{correction_info}{
		Logical value. if \code{TRUE} compute the variable "correction" : percentage of missing age cases.
}
  \item{var_asr}{
		Name of the new variable for the ASR.
}
  \item{age_dropped}{
		Only for truncated ASR.\cr
		Logical value: if \code{TRUE}, Assume the age group not used are already dropped from the data.\cr
		First_age and last_age option \bold{must} still be filled.
}
}
\details{
	This function take automatically account the number of age group (last age group can be 70+,75+,80+,85+):\cr
	If the population data stops before 85+ (75+ for instance), the population data \bold{must} be 0 when the population data is unknown so, the program can detect automatically the last age group (70+,75+,80+ or 85+) for population.

}
\value{
	Give the list of population with less than 18 age group (last age group: 70+, 75+, 80+).\cr
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
	\code{\link{csu_cumrisk}}
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
				    		  
}




