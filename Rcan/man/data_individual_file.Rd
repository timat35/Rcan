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
    \item{\code{beh}}{behaviour code}
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


data(ICD_group_file)
data(data_individual_file)

#group individual data by 
#	5 year age group 

df_data_age <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "site","reglabel"))


#group individual data by 
#	5 year age group 
#	ICD grouping from dataframe ICD_group_file

df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode","reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site") 

}
\keyword{datasets}
