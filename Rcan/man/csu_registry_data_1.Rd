\name{csu_registry_data_1}
\alias{csu_registry_data_1}
\docType{data}
\title{
cancer registry data 
}
\description{
Cancer registry data for liver cancer, males, 2007, 4 registries. 
}
\usage{data("csu_registry_data_1")}
\format{
  A data frame with 76 observations on the following 5 variables.
  \describe{
    \item{\code{age}}{age variable from 1 to 19. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+, 19 represents missing age.} 
    \item{\code{cases}}{Number of cases (incidence)}
    \item{\code{py}}{Population-year: Reference population of the registry }
    \item{\code{registry_label}}{Name of the registry}
    \item{\code{registry}}{Registry code}
  }
}
\details{
this 4 registries have been selected for this example as they  different number of age group (75+, 80+, 85+) and some have missing age cases.
}
\source{
\url{http://ci5.iarc.fr/Default.aspx}
}

\seealso{
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
}

\examples{

data(csu_registry_data_1)

# Age standardized rate (ASR) with no missing age cases.
result <- csu_asr(csu_registry_data_1, 
                  "age", "cases", "py",
                  group_by = c("registry", "registry_label" ),
                  var_age_group = c("registry_label"))
				  
#See more examples here:
help(csu_asr)

				  
				  
}
\keyword{datasets}
