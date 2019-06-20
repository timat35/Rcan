\name{csu_CI5XI_data}
\alias{csu_CI5XI_data}
\docType{data}
\title{
Cancer in five continent volume XI.
}
\description{
 CI5 XI summary database.
}
\usage{data("csu_CI5XI_data")}
\format{
  A data frame with 1113210 observations on the following 10 variables.
  \describe{
  	\item{\code{registry_code}}{Registry code (integer)}
    \item{\code{registry_label}}{Name of the registry (character)}
    \item{\code{sex}}{sex (integer)}
    \item{\code{cancer_code}}{cancer code (integer)}
    \item{\code{cancer_label}}{cancer label (character)}
    \item{\code{age}}{age variable from 1 to 19. 1 is 0-4 years, 2 is 5-9, etc..., 17 is 80-84, 18 is 85+, 19 represents missing age. (integer)} 
    \item{\code{cases}}{Number of cases (incidence) (integer)}
    \item{\code{py}}{Population-year: Reference population of the registry (integer)}
    \item{\code{CI5_continent}}{Continent code (integer)}
    \item{\code{ethnic_group}}{ethnic code (integer)}
  }
}
\details{
All information are available here: \url{http://ci5.iarc.fr/CI5-XI/Default.aspx}
}
\source{
\url{http://ci5.iarc.fr/CI5-XI/Default.aspx}
}

\seealso{
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
}

\examples{

data("csu_CI5XI_data")

#get the registry code asssociate to registry_label
#print(unique(csu_CI5XI_data[,c("registry_code", "registry_label" )]),nrows = 1000)

#get the cancer code asssociate to cancer_label
#print(unique(csu_CI5XI_data[,c("cancer_label", "cancer_code")]),nrows = 1000)

#remove all cancers:
df_data <- subset(csu_CI5XI_data ,cancer_code < 62)

#change group_by option to factor
df_data$sex <- factor(df_data$sex, levels=c(1,2), labels=c("Male", "Female"))


#select Thailand changmai
df_data_1 <- subset(df_data, registry_code==764001)



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
df_data_2 <- subset(df_data,registry_code== 840000 & ethnic_group == 99)

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
