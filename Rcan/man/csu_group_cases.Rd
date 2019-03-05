\name{csu_group_cases}
\alias{csu_group_cases}
\title{csu_group_cases}

\description{
\code{csu_group_case} Group individual data into 5 years age-group data across different population (year, sex, cancer). Optionally: Group cancer based on a standard ICD group file; Extract year from custom year format.
}
\usage{
csu_group_cases(df_data, 
	var_age ,
	cross_by=NULL,
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
  \item{cross_by}{
		(Optional) A vector of variables to create the all possible category for each age group (and ICD and Year in option).\cr
		For instance sex (1, 2) and country code (250, 356, 840) would create six category:
		\tabular{ll}{
			sex \tab country_code\cr
			1 \tab 250\cr
			1 \tab 356\cr
			1 \tab 840\cr
			2 \tab 250\cr
			2 \tab 356\cr
			2 \tab 840\cr
		}
}
  \item{group_by}{
		(Optional) A vector of other relative variables to keep.\cr
		For instance, sex label, cancer label (to avoid create false category such as "1 male", "1 female", etc..)
}
  \item{var_cases}{
		(Optional) cases variable: If there is already a variable for the number for cases.
}
  \item{df_ICD}{
	(Optional) ICD file for ICD grouping information. Must have 2 field: "ICD", "LABEL"\cr.
	This will regroup the ICD by LABEL, and update ICD code.\cr
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
}
  \item{var_ICD}{
	(Optional) ICD variable: ICD variable in the individual data 
}
  \item{var_year}{
	(Optional) Year variable: Extract year from custom format ("yyyymmdd","ddmmyyy", "yyyy/mm","dd-mm-yyyy", etc..) and group data by year.
}
}

\details{
	This function group individual data to allow further analysis\cr 
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

data(csu_registry_data_2)

# you import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# Estimated Annual Percentage Change (EAPC) base on ASR.

\donttest{
df_asr <- 
	csu_asr(csu_registry_data_2, 
	  "age", "cases", "py",
	  group_by = c("registry", "registry_label", "sex", "year", "ethnic" ),
	  var_age_group = c("registry_label"), 
	  missing_age = 99
	  )
				  
result <- 
	csu_eapc(df_asr,
		"asr", "year",
		group_by=c("registry", "registry_label", "sex", "ethnic" )
		)

}					
# you can export your result as csv file using write.csv:
# write.csv(result, file="result.csv")
				  		  
}




