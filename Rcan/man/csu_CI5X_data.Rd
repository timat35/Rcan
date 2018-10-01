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
    \item{\code{age}}{age variable from 1 to 19. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+, 19 represents missing age. (integer)} 
    \item{\code{cases}}{Number of cases (incidence) (integer)}
    \item{\code{py}}{Population-year: Reference population of the registry (integer)}
  }
}
\details{
All information are available here: \url{http://ci5.iarc.fr/Default.aspx}
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

data("csu_CI5X_data")

#get the registry code asssociate to registry_label
print(unique(csu_CI5X_data[,c("registry_label", "registry_code")]),nrows = 1000)

#get the cancer code asssociate to cancer_label
print(unique(csu_CI5X_data[,c("cancer_label", "cancer_code")]),nrows = 1000)

#remove all cancers:
df_data <- csu_CI5X_data[csu_CI5X_data$cancer_code < 62,]


#select Thailand changmai
df_data_1 <- df_data[df_data$registry_code==76401,]






dt_result_1 <- csu_ageSpecific_top(df_data_1,
                    var_age="age", 
                    var_cases="cases", 
                    var_py="py",
                    var_top="cancer_label", 
                    group_by="sex", 
                    plot_title= "Thailand, Chiangmai",
                    plot_subtitle = "Top 5 cancer",
                    missing_age = 19)



\donttest{					
#select USAm NPCR
df_data_2 <- df_data[df_data$registry_code== 84080,]

dt_result_2 <- csu_ageSpecific_top(df_data_2,
                                   var_age="age", 
                                   var_cases="cases", 
                                   var_py="py",
                                   var_top="cancer_label", 
                                   group_by="sex", 
                                   plot_title= "USA, NPCR",
                                   plot_subtitle = "Top 5 cancer",
                                   missing_age = 19)


				  
}			  
}
\keyword{datasets}
