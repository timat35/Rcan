\name{data_individual_file}
\alias{data_individual_file}
\docType{data}
\title{
	Data individual example
}
\description{
	Data individual example for the function \code{\link{csu_group_cases}}\cr
	1 line = 1 cases.
}
\usage{data("data_individual_file")}
\format{
  A data frame with 19284 observations on the following 9 variables.
  \describe{
    \item{\code{age}}{age}
    \item{\code{sex}}{sex}
    \item{\code{regcode}}{registry code}
    \item{\code{reglabel}}{registry label}
    \item{\code{site}}{ICD10 code}
    \item{\code{doi}}{date of birth (yyyymmdd)}
    \item{\code{histo}}{histology}
    \item{\code{grade}}{grade}
    \item{\code{basis}}{basis}
  }
}
\details{
This dataset provide an example how to group individual cases to 5 years data, grouped bu ICD code and year using the function \code{\link{csu_group_cases}} and the database \code{\link{ICD_group_file}} 
}
\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{ICD_group_file}}
}

\examples{

data(csu_registry_data_1)
test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male",
                CI5_comparison = "Liver")
 
#See more examples here:
help(csu_ageSpecific)

}
\keyword{datasets}
