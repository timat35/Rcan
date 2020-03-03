\name{csu_registry_data_2}
\alias{csu_registry_data_2}
\docType{data}
\title{
cancer registry data 
}
\description{
Cancer registry data for liver cancer
}
\usage{data("csu_registry_data_2")}
\format{
  A data frame with 125856 observations on the following 8 variables.
  \describe{
	\item{\code{sex}}{sex variable: 1 male, 2 female}
	\item{\code{year}}{year variable, from 1953 to 2007}
    \item{\code{age}}{age variable from 0 to 85 with missing age.\cr
	0 is 0-4 years, 5 is 5-9, ..., 80 is 80-84, 85 is 85+, 99 represents missing age.} 
    \item{\code{cases}}{Number of cases (incidence)}
    \item{\code{py}}{Population-year: Reference population of the registry }
    \item{\code{registry_label}}{Name of the registry (118 populations)}
    \item{\code{registry}}{Registry code (102 registries)}
	\item{\code{ethnic}}{ethnic code: white (10), black (30), ..., all(99).}
  }
}

\source{
\url{http://ci5.iarc.fr/Default.aspx}
}

\seealso{
  \code{\link{csu_asr}}
  \code{\link{csu_cumrisk}}
  \code{\link{csu_eapc}}
  \code{\link{csu_ageSpecific}}
  \code{\link{csu_ageSpecific_top}}
  \code{\link{csu_bar_top}}
  \code{\link{csu_time_trend}}
  \code{\link{csu_trendCohortPeriod}}
}

\examples{

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
				  
}
\keyword{datasets}
