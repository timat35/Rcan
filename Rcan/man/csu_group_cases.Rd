\name{csu_group_cases}
\alias{csu_group_cases}
\title{csu_group_cases}

\description{
\code{csu_group_cases} groups individual data into 5 years age-group data and other user defined variable (sex, registry, etc...).\cr 
Optionally: Group cancer based on a standard ICD10 coding; Extract year from custom year format.
}
\usage{
csu_group_cases(df_data, 
	var_age ,
	group_by=NULL,
	var_cases = NULL,
	df_ICD = NULL,
	var_ICD=NULL,
	var_year = NULL,
	all_cancer=FALSE) 
	}

\arguments{
  \item{df_data}{
  Individual data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_age}{
		Age variable. (Numeric). Value > 150 will be consider as missing age.
	}
  \item{group_by}{
		(Optional) A vector of variables to create the different population (sex, country, etc...).\cr
	}
  \item{var_cases}{
		(Optional) cases variable: If there is already a variable for the number of cases.
	}
  \item{df_ICD}{
	(Optional) ICD file for ICD grouping information. Must have 2 fields: "ICD", "LABEL"\cr.
	2 format are possible:\cr
	Each ICD code separate by ICD group\cr
	\tabular{ll}{
		ICD \tab LABEL\cr
		C82 \tab NHL\cr
		C83 \tab NHL\cr
		C84 \tab NHL\cr
		C85 \tab NHL\cr
		C96 \tab NHL\cr
	}
	ICD code already grouped.\cr
	\tabular{ll}{
		ICD_group \tab LABEL\cr
		C82-85,C96 \tab NHL\cr
	}
	2 ICD code separate by "-" includes all the ICD code between.\cr
	2 ICD code separate by "," includes only these 2 ICD code.\cr
	For instance, C82-85, C96 (or C82-C85, C96) includes: \cr
	C82, C83, C84, C85 and C96 \cr
	Must be filled if \code{var_ICD} argument is defined
	\cr

	example: \code{\link{ICD_group_GLOBOCAN}}
}
  \item{var_ICD}{
	(Optional) ICD variable: ICD variable in the individual data.\cr
	Must be filled if \code{df_ICD} argument is defined
}
  \item{var_year}{
	(Optional) Year variable: Extract year from custom format ("yyyymmdd","ddmmyyyy", "yyyy/mm","dd-mm-yyyy", etc..) and group data by year.
}
  \item{all_cancer}{
	(Optional) If \code{TRUE}, will calculate the number of cases for all cancers (C00-97) and all cancers but non-melanoma of skin (C00-97 but C44)\cr
	Need \code{var_ICD} and \code{df_ICD} argument to be defined 
}
}

\details{
	For most analysis, individual cases database need to be grouped by category.\cr 
	This function groups data by 5 years age-group and other user defined variable.\cr 
	Next step will be to add 5 years population data.
}
\value{
	Return a dataframe.
	
}
\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_asr}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
	\code{\link{csu_eapc}}
}

\examples{

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

data(ICD_group_GLOBOCAN)
data(data_individual_file)

#group individual data by 
#	5 year age group 

df_data_age <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "site","reglabel")) 

#group individual data by 
#	5 year age group 
#	ICD grouping from dataframe ICD_group_GLOBOCAN

df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode","reglabel"),
	df_ICD = ICD_group_GLOBOCAN,
	var_ICD  ="site") 

#group individual data by 
#	5 year age group 
#	ICD grouping from dataframe ICD_group_GLOBOCAN
#	year (extract from date of incidence)

df_data_year <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode","reglabel"),
	df_ICD = ICD_group_GLOBOCAN,
	var_ICD  ="site",
	var_year = "doi") 			

# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  		  
}




