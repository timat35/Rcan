\name{ICD_group_file}
\alias{ICD_group_file}
\docType{data}
\title{
	ICD10 group example
}
\description{
	ICD10 group example for the function \code{\link{csu_group_cases}}
}
\usage{data("ICD_group_file")}
\format{
  A data frame with 97 observations on the following 2 variables.
  \describe{
    \item{\code{ICD}}{ICD10 code}
    \item{\code{LABEL}}{label for cancer group}
  }
}
\details{
This dataset provide an example how to regroup ICD code using the function \code{\link{csu_group_cases}}
For instance this group
\tabular{ll}{
	ICD \tab LABEL\cr
	C18 \tab COLORECTUM\cr
	C19 \tab COLORECTUM\cr
	C20 \tab COLORECTUM\cr
	C21 \tab COLORECTUM\cr
}

Will become:
\tabular{ll}{
	ICD_group \tab LABEL\cr
	C18-C21 \tab COLORECTUM\cr
}

See: \code{\link{csu_group_cases}}
}
\seealso{
	\code{\link{csu_group_cases}}
}

\examples{


data(ICD_group_file)
data(data_individual_file)

#group individual data by 
#	5 year age group 
df_data_age <- csu_group_cases(data_individual_file,
	var_age="age",
	cross_by=c("sex", "regcode", "site"),
	group_by=c("reglabel")) 

#group individual data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file
df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	cross_by=c("sex", "regcode"),
	group_by=c("reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site") 

#group individual data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file
#	year (extract from date of incidence)
df_data_year <- csu_group_cases(data_individual_file,
	var_age="age",
	cross_by=c("sex", "regcode"),
	group_by=c("reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site",
	var_year = "doi") 
}
\keyword{datasets}
