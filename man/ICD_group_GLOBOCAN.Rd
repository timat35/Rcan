\name{ICD_group_GLOBOCAN}
\alias{ICD_group_GLOBOCAN}
\docType{data}
\title{
	ICD10 group example
}
\description{
	ICD10 group example for the function \code{\link{csu_group_cases}} based on GLOBOCAN
}
\usage{data("ICD_group_GLOBOCAN")}
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
	\code{\link{ICD_group_CI5}}
}

\examples{


data(ICD_group_GLOBOCAN)
data(data_individual_file)

#group individual data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_GLOBOCAN

df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "reglabel"),
	df_ICD = ICD_group_GLOBOCAN,
	var_ICD  ="site") 
}
\keyword{datasets}
