\name{csu_eapc}
\alias{csu_eapc}
\title{csu_eapc}

\description{
\code{csu_eapc} calculate the Estimated Annual Percentage Change (EAPC) of rates during a time period with the Confidence Interval (CI) across different population (Registry, year, sex...)
}
\usage{
csu_eapc(df_data,
	var_rate="asr",
	var_year="year",
	group_by=NULL,
	var_eapc = "eapc",
	CI_level = 0.95)
}

\arguments{
  \item{df_data}{
  Data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_rate}{
		Rate variable. (Standardized or not, incidence, mortality, etc..)
}
  \item{var_year}{
		Period variable. (Year, month, etc...)
}
  \item{group_by}{
		A vector of variables to compare different EAPC (sex, country, cancer ...).\cr
}
  \item{var_eapc}{
		Name of the new variable for the EAPC.\cr
		The variable for the CI will be name based on \code{var_eapc} + "_" +  up/Low.
}
  \item{CI_level}{
		Confidence interval level. Default is 0.95.
}

}
\details{
	This function use Generalized Linear Model (GLM):\cr
	glm(log(rate) ~ year, family=gaussian(link = "identity")). \cr 
	0 value are ignored. More details in reference below.\cr

	
}
\value{
	Return a dataframe.
	
}
\references{
	\url{https://regstattools.iconcologia.net/stats/sart/eapc/eapc_method.pdf}
}
\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{csu_merge_cases_pop}}
	\code{\link{csu_asr}}
	\code{\link{csu_cumrisk}}
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_ageSpecific_top}}
	\code{\link{csu_bar_top}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
}

\examples{

data(csu_registry_data_2)

# you import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# Estimated Annual Percentage Change (EAPC) base on ASR.

\donttest{
df_asr <- 
	csu_asr(csu_registry_data_2, 
	  "age", "cases", "py",
	  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
	  var_age_group = c("registry_label"), 
	  missing_age = 99
	  )
				  
result <- 
	csu_eapc(df_asr,
		"asr", "year",
		group_by=c("registry", "registry_label", "sex", "ethnic" )
		)

}					
# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  		  
}




