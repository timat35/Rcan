\name{csu_CI5X_data}
\alias{csu_CI5X_data}
\docType{data}
\title{
Cancer in five continent volume X.
}
\description{
 CI5 X summary database.
}
\usage{data("csu_CI5X_data")}
\format{
  A data frame with 1015056 observations on the following 11 variables.
  \describe{
  	\item{\code{registry_code}}{Registry code (integer)}
    \item{\code{registry_label}}{Name of the registry (factor)}
	\item{\code{ethnic_group}}{ethnic code (integer)}
	\item{\code{sex}}{sex (factor)}
	\item{\code{cancer_code}}{cancer code (integer)}
	\item{\code{cancer_label}}{cancer label (factor)}
	\item{\code{mv}}{Microscopic Verification (\%) (integer)}
	\item{\code{dco}}{Death Certificate Only (\%) (integer)}
    \item{\code{age}}{age variable from 1 to 19. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+, 19 represente missing age. (integer)} 
    \item{\code{cases}}{Number of cases (incidence) (integer)}
    \item{\code{py}}{Population-year: Reference population of the registry (integer)}
  }
}
\details{
All information are available here: \url{ http://ci5.iarc.fr/CI5-X/Default.aspx}
}
\source{
\url{ http://ci5.iarc.fr/CI5-X/Pages/download.aspx}
}

\seealso{
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
}

\examples{

data(csu_CI5X_data)

# Age standardized rate (ASR) with no missing age cases.

				  
#See more examples here:
help(csu_asr)

				  
				  
}
\keyword{datasets}
