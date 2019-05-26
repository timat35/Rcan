\name{data_population_file}
\alias{data_population_file}
\docType{data}
\title{
	Population data example
}
\description{
	Population data example for the function \code{\link{csu_merge_cases_pop}}.
}
\usage{data("data_population_file")}
\format{
  A data frame with 160 observations on the following 9 variables.
  \describe{
    \item{\code{year}}{year}
    \item{\code{sex}}{sex}
    \item{\code{age_group}}{5 year age group}
    \item{\code{pop}}{Count of population year}
  }
}
\details{
This dataset provide an example to merge 5 years age grouped data with population data using the function \code{\link{csu_merge_cases_pop}}
}
\seealso{
	\code{\link{csu_merge_cases_pop}}
}

\examples{


data(ICD_group_GLOBOCAN)
data(data_individual_file)
data(data_population_file)

#group individual data by 
# 5 year age group 
# ICD grouping from dataframe ICD_group_GLOBOCAN
# year (extract from date of incidence)

df_data_year <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site",
  var_year = "doi")     

#Merge 5-years age grouped data with population by year (automatic) and sex

df_pop <- csu_merge_cases_pop(df_data_year, data_population_file, var_age = "age_group", 
                           var_cases = "cases", var_py = "pop", group_by = c("sex"))
}
\keyword{datasets}
