\name{csu_CI5XII_data}
\alias{csu_CI5XII_data}
\docType{data}
\title{
Cancer in five continent volume XII.
}
\description{
 CI5 XII summary database.
}
\usage{data("csu_CI5XII_data")}
\format{
  A data frame with 1113210 observations on the following 10 variables.
  \describe{
  	\item{\code{id_code}}{population code (integer)}
    \item{\code{id_label}}{population label (character)}
    \item{\code{country_code}}{UN country code (integer)}
    \item{\code{ethnic_code}}{ethnic code (integer)}
    \item{\code{cancer_code}}{cancer code (integer)}
    \item{\code{cancer_label}}{cancer label (character)}
    \item{\code{sex}}{sex (integer)}
    \item{\code{age}}{age variable from 1 to 19. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+, 19 represents missing age. (integer)} 
    \item{\code{cases}}{Number of cases (incidence) (integer)}
    \item{\code{py}}{Population-year: Reference population of the registry (integer)}
    \item{\code{period}}{Period (character)}
    
  }
}
\details{
All information are available here: \url{https://ci5.iarc.fr/ci5-xii/}
}
\source{
\url{https://ci5.iarc.fr/ci5-xii/}
}

\seealso{
	\code{\link{csu_asr}}
  \code{\link{csu_cumrisk}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
}

\examples{

data("csu_CI5XII_data")

#get the id code asssociate to id_label
#print(unique(csu_CI5XII_data[,c("id_code", "id_label" )]),nrows = 1000)

#get the cancer code asssociate to cancer_label
#print(unique(csu_CI5XII_data[,c("cancer_label", "cancer_code")]),nrows = 1000)

#remove all cancers:
df_data <- subset(csu_CI5XII_data ,cancer_code < 62)

#change group_by option to factor
df_data$sex <- factor(df_data$sex, levels=c(1,2), labels=c("Male", "Female"))


#select Thailand changmai
df_data_1 <- subset(df_data, id_code==476400199)



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
#select USA
df_data_2 <- subset(df_data,id_code== 384008099 & ethnic_code == 99)

dt_result_2 <- csu_ageSpecific_top(df_data_2,
                                   var_age="age", 
                                   var_cases="cases", 
                                   var_py="py",
                                   var_top="cancer_label", 
                                   group_by="sex", 
                                   plot_title= "USA",
                                   plot_subtitle = "Top 5 cancer",
                                   missing_age = 19)


				  
}			  
}
\keyword{datasets}
