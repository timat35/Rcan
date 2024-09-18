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
  A data frame with 19284 observations on the following 10 variables.
  \describe{
    \item{\code{regcode}}{registry code}
    \item{\code{reglabel}}{registry label}
    \item{\code{sex}}{sex}
    \item{\code{age}}{age}
    \item{\code{doi}}{date of birth (yyyymmdd)}
    \item{\code{site}}{ICD10 code}
    \item{\code{histo}}{histology}
    \item{\code{beh}}{behavior code}
    \item{\code{grade}}{grade}
    \item{\code{basis}}{basis}
  }
}
\details{
This dataset provide an example how to group individual cases to 5 years data, grouped by ICD code and year using the function \code{\link{csu_group_cases}} and the database \code{\link{ICD_group_GLOBOCAN}} 
}
\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{ICD_group_GLOBOCAN}}
  \code{\link{ICD_group_CI5}}
}

\examples{


data(ICD_group_GLOBOCAN)
data(data_individual_file)

#group individual data by 
#	5 year age group 

df_data_age <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "reglabel", "site"))


#group individual data by 
#	5 year age group 
#	ICD grouping from dataframe ICD_group_GLOBOCAN

df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "reglabel"),
	df_ICD = ICD_group_GLOBOCAN,
	var_ICD  ="site") 

}
\keyword{datasets}
