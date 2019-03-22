\name{csu_group_cases}
\alias{csu_group_cases}
\title{csu_group_cases}

\description{
\code{csu_group_case} group individual data into 5 years age-group data and other user defined variable (sex, registry, etc...).\cr 
Optionally: Group cancer based on a standard ICD10 coding; Extract year from custom year format.
}
\usage{
csu_group_cases(df_data, 
	var_age ,
	group_by=NULL,
	var_cases = NULL,
	df_ICD = NULL,
	var_ICD=NULL,
	var_year = NULL) 
	}

\arguments{
  \item{df_data}{
  Individual data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_age}{
		Age variable. (Numeric). Value > 150 will be consider as missing age.
	}
  \item{group_by}{
		(Optional) A vector of variables to create the different population (sex, country, etc..).\cr
	}
  \item{var_cases}{
		(Optional) cases variable: If there is already a variable for the number for cases.
	}
  \item{df_ICD}{
	(Optional) ICD file for ICD grouping information. Must have 2 field: "ICD", "LABEL"\cr.
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
	example: \code{\link{ICD_group_file}}
}
  \item{var_ICD}{
	(Optional) ICD variable: ICD variable in the individual data. 
}
  \item{var_year}{
	(Optional) Year variable: Extract year from custom format ("yyyymmdd","ddmmyyyy", "yyyy/mm","dd-mm-yyyy", etc..) and group data by year.
}
}

\details{
	For most analysis, individual cases database need to be grouped by category.\cr 
	This function group data by 5 years age-group and other user defined variable.\cr 
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

data(ICD_group_file)
data(data_individual_file)

#group individual data by 
#	5 year age group 

df_data_age <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode", "site","reglabel")) 

#group individual data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file

df_data_icd <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode","reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site") 

#group individual data by 
#	5 year age group 
#	ICd grouping from dataframe ICD_group_file
#	year (extract from date of incidence)

df_data_year <- csu_group_cases(data_individual_file,
	var_age="age",
	group_by=c("sex", "regcode","reglabel"),
	df_ICD = ICD_group_file,
	var_ICD  ="site",
	var_year = "doi") 			

# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  		  
}




