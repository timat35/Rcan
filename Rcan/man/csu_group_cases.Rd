\name{csu_group_case}
\alias{csu_group_case}
\title{csu_group_case}

\description{
\code{csu_group_case} Group individual data into 5 years age-group data across different population (year, sex, cancer). Optionally: Group cancer based on a standard ICD group file; Extract year from custom year format.
}
\usage{
csu_group_case(df_data, 
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
		(Optional) A vector of variables to create the all possible category for each age group (ICD and Year in option).\cr
		For instance sex (1, 2) and cancer (C15, C16, C17) would create six category (C15 1, C15 2, ..., C17 1, C17 2) for each age group.\cr

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
	"C18" Colorectum, "C19" Colorectum, "C20" Colorectum, "C21" Colorectum will be group as "C18-21" Colorectum.
}
  \item{var_ICD}{
	(Optional) ICD variable: ICd variable in the individual data 
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




