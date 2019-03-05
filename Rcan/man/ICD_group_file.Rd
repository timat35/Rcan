\name{ICD_group_file}
\alias{ICD_group_file}
\docType{data}
\title{
	ICD10 group example
}
\description{
	ICD10 group example for the function \code{\link{csu_group_cases}}
}
\usage{data("csu_ci5x_mean")}
\format{
  A data frame with 97 observations on the following 2 variables.
  \describe{
    \item{\code{CSU_age_factor}}{age variable from 1 to 18. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+.} 
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

data(csu_registry_data_1)
test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]
csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male",
                CI5_comparison = "Liver")
 
#See more examples here:
help(csu_ageSpecific)

}
\keyword{datasets}
