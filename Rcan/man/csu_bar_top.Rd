\name{csu_bar_top}
\alias{csu_bar_top}
\title{csu_bar_top}

\description{
\code{csu_bar_top} plots top X single-sided or double-sided bar chart.
}
\usage{
csu_bar_top(df_data,
	var_value, 
	var_bar,
	group_by=NULL,
	nb_top = 10,
	plot_title=NULL,
	plot_subtitle=NULL,
	xtitle= NULL,
	label_by=NULL,
	color=NULL,
	digits = 1)
}

\arguments{
  \item{df_data}{
  Data (need to be R \code{data.frame} format, see examples to import csv file).
	}
  \item{var_value}{
	Value variable.\cr
	There must be only 1 value for each bar.
	}
  \item{var_bar}{
	Bar label variable.
	}
  \item{group_by}{
  	\itemize{
  		\item Single-sided bar chart. \code{NULL} (default)
  		\item Double-sided bar chart. Variable name with exactly 2 values. (For example, "sex").
  	}
	Must be filled if \code{label_by} argument is defined.
	}
   \item{nb_top}{
		Lowest Rank included. Default is 10.
	}
  \item{plot_title}{
	Title of the plot. (For example, "Top 10 cancer sites").
	}
  \item{plot_subtitle}{
	Subtitle of the plot. (For example, "Males").
	}
	\item{xtitle}{
	x-axe title. (For example, "Number of cases").
	}
	\item{label_by}{
	2 values vector. Will overwrite the legend label for double-sided bar chart. (See \code{group_by}).\cr
	For example: c("Male", "Female"). 
	}
  \item{color}{
  	Color codes are hexadecimal.\cr
	\itemize{
  		\item Single-sided bar chart. 1 hexadecimal color code (same color for each bar) or variable name with a color associated to each bar label variable. 
  		\item Double-sided bar chart. 2 values vector. For example: c("#2c7bb6","#b62ca1").
  	}
	}
	\item{digits}{
	Number of decimal digits. Default: 1
	}
 
}
\details{
	This function plots a top X (default is top 10) bar chart, single-sided or double sided.
}
\value{
	Return plots and a \code{data.frame}.
	
}
\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{csu_merge_cases_pop}}
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_ageSpecific_top}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
}

\examples{

data(data_individual_file)
data(data_population_file)
data(ICD_group_GLOBOCAN)

#Group individual data by:
#5 year age group
#ICD grouping from dataframe ICD_group_GLOBOCAN
#year extract from date of incidence

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

# calculate asr
df_asr <- csu_asr(df_data,
  "age_group", 
  "cases",
  "pop",
  group_by=c("sex", "ICD_group", "LABEL"),
  missing_age =19)

#remove Other cancer
df_asr <- df_asr[df_asr$LABEL != "Other",]
df_asr <- df_asr[df_asr$LABEL != "Other skin",]

#keep male
df_asr_M <- df_asr[df_asr$sex==1,]

#Single sided bar plot 
data1 <- csu_bar_top(
   df_asr_M,
   var_value="cases",
   var_bar="LABEL",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   xtitle= "Number of cases",
   color= c("#2c7bb6"),
   digits=0) 

#Double sided bar plot example 1
data2 <- csu_bar_top(
   df_asr,
   var_value="cases",
   var_bar="LABEL",
   group_by="sex",
   nb_top = 15,
   plot_title = "Top 15 cancer sites",
   xtitle= "Number of cases",
   label_by=c("Male", "Female"),
   color = c("#2c7bb6","#b62ca1"),
   digits=0) 

#Double sided bar plot example 2
data3 <- csu_bar_top(
   df_asr,
   var_value="asr",
   var_bar="LABEL",
   group_by="sex",
   nb_top = 10,
   plot_title = "Top 10 cancer sites",
   xtitle= "Age-standardized rate per 100,000",
   label_by=c("Male", "Female"),
   color = c("#2c7bb6","#b62ca1"),
   digits=1) 

}




