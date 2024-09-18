\name{csu_merge_cases_pop}
\alias{csu_merge_cases_pop}
\title{csu_merge_cases_pop}

\description{
\code{csu_merge_cases_pop} merges registry data and population data, group by year and other user defined variable (sex, registry, etc...).\cr 
}
\usage{
csu_merge_cases_pop(df_cases, 
	df_pop,
	var_age,
	var_cases="cases",
	var_py=NULL,
	group_by=NULL) 
	}

\arguments{
  \item{df_cases}{
  		Registry data group by 5 years-age group (need to be R \code{data.frame} format, see examples to import csv file).
	}
  \item{df_pop}{
  		Population data group by 5-years age group (need to be R \code{data.frame} format, see examples to import csv file).
	}
  \item{var_age}{
		Age variable. Several format are accepted 
		\tabular{lll}{
			1 \tab 0-4 \tab 0 \cr
			2 \tab 5-9 \tab 5 \cr
			3 \tab 10-14 \tab 10 \cr
			... \tab ... \tab ... \cr
			17 \tab 80-84 \tab 80 \cr
			18 \tab 85+ \tab 85 \cr
		}
		\bold{This variable must be a variable with the same column name in both dataset } (\code{df_cases} and \code{df_pop}).\cr
		Age >= 85 in the \code{df_pop} dataset will be aggregated as 85+.
	}
  \item{var_cases}{
		Cases variable in the \code{df_cases} dataset.\cr
	}
  \item{var_py}{
		(Optional) If population is "long format", name of the population variable in the \code{df_pop} dataset.\cr
		If population data is wide format (see \code{details}), var_py must be NULL.
	}
  \item{group_by}{
		(Optional) A vector of variables to create the different population (sex, country, etc...).\cr
		\bold{Each variable must be a variable with the same column name in both dataset } (\code{df_cases} and \code{df_pop}).\cr
		Do not include the "year" variable since it is automatically detected (see details).
	}
}

\details{
	This function merges registry data and population for further analysis.\cr 
	Both datasets must be group by 5-years age group.\cr
	\bold{If present, the year information in format "yyyy" will be detected automatically}.\cr 
	2 formats are accepted for population data:.\cr
	Long format: (year and population are 2 variables)\cr
	\tabular{llll}{
		sex	\tab age \tab pop \tab year \cr
		1 \tab 1 \tab 116128 \tab 2005 \cr
		1 \tab 2 \tab 130995 \tab 2005 \cr
		1 \tab 3 \tab 137556 \tab 2005 \cr
		... \tab ... \tab ... \tab ... \cr
		2 \tab 16 \tab 27171 \tab 2007 \cr
		2 \tab 17 \tab 13585 \tab 2007 \cr
		2 \tab 18 \tab 13585 \tab 2007 \cr
	}
	Wide format: (One column per year and no population variable, "yyyy" year format must be included in columns name)\cr
	\tabular{lllll}{
		sex	\tab age \tab Y2013 \tab Y2014 \tab Y2015 \cr
		1 \tab 0-4 \tab 215607 \tab 237346 \tab 247166 \cr
		1 \tab 5-9 \tab 160498 \tab 152190 \tab 152113 \cr
		1 \tab 10-14 \tab 175676 \tab 171794 \tab 165406 \cr
		... \tab ... \tab ... \tab ... \tab ... \cr
		2 \tab 75-79 \tab 20625 \tab 20868 \tab 23434 \cr
		2 \tab 80-84 \tab 7187 \tab 7276 \tab 7620 \cr
		2 \tab 85+ \tab 2551 \tab 2597 \tab 2617 \cr
	}
}

\value{
	Return a dataframe.
	
}
\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{csu_asr}}
	\code{\link{csu_cumrisk}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_ageSpecific_top}}
	\code{\link{csu_bar_top}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
}
\examples{

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

data(ICD_group_GLOBOCAN)
data(data_individual_file)
data(data_population_file)

#group individual data by 
# 5 year age group 
# ICD grouping from dataframe ICD_group_GLOBOCAN
# year (extract from date of incidence)

df_data_year <- csu_group_cases(data_individual_file,
  var_age="age",
  group_by=c("sex", "regcode", "reglabel"),
  df_ICD = ICD_group_GLOBOCAN,
  var_ICD  ="site",
  var_year = "doi")     

#Merge 5-years age grouped data with population by year (automatic) and sex

df_data <- csu_merge_cases_pop(
	df_data_year, 
	data_population_file, 
	var_age = "age_group",
	var_cases = "cases",
	var_py = "pop",
	group_by = c("sex"))


# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  		  
}




