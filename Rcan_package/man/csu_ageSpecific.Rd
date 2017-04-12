\name{csu_ageSpecific}
\alias{csu_ageSpecific}
\title{csu_ageSpecific}

\description{
\code{csu_ageSpecific} calculate and plot Age-Specific Rate (ASR).
}
\usage{
csu_ageSpecific(df_data,
		var_age="age",
		var_cases="cases",
		var_py="py",
		var_by = NULL,
		missing_age = NULL,
		db_rate = 100000,
		plot_title = "csu_title",
		legend = csu_trend_legend(),
		format_export = NULL,
		draw_plot = TRUE,
		return_rate = FALSE,
		var_rate = "rate",
		CI5_comparaison = NULL)
}
  
\arguments{
  \item{df_data}{
  Data (need to be R dataframe format, see example to import csv file).
	}
  \item{var_age}{
	(Optional, String) Age group variable: (numerical) 18 5-years age-group, 0-4, 5-9, .. 85+ and an optional missing age.\cr
	The value can be 1,2..19, as well as 0,5,..85 or other value as long are they are numerical.\cr
	Default is "age".
	}
  \item{var_cases}{
		(Optional, String) Number of cases variable (incidence).\cr
		Default is "cases".
	}
  \item{var_py}{
		(Optional, String) Population year variable.\cr
		Default is "py".
	}
  \item{var_by}{
		(Optional) Variable to compare different age specific rate (sex, country, cancer ...).\cr
		Only one variable can be choose.
	}
  \item{missing_age}{
		(Optional) Age value representing the missing age cases.
	}
  \item{db_rate}{
		(Optional) The denominator population. Default is 100000.
	}
  \item{plot_title}{
		(Optional) Title of the plot. Default is csu_title.
	}
  \item{legend}{
		(Optional) legend option: see \code{\link{csu_trend_legend}}.\cr
		Use only if \code{var_by} not NULL
	}
  \item{format_export}{
		(Optional) export format 2 option: "pdf" or "svg". The filename is the \code{plot_title} option.
	}
  \item{draw_plot}{
		(Optional) To show plot. Default is TRUE.
	}
  \item{return_rate}{
		(Optional) To return the age specific rate in a dataframe. Default is FALSE
	}
  \item{var_rate}{
		(Optional) name of the age specific variable if a dataframe is return. Default is "rate"
	}
  \item{CI5_comparaison}{
		(Optional) Add a dotted line represnting the CI5X for a specific cancer.\cr
		Value can be a cancer_label (exemple: "Liver"), or a cancer_code (exemple = 7).\cr
		See \code{\link{csu_ci5x_mean}} to get accepted values.
	}
}
\details{
	This function calculate and plot the age specific rate. 
	The \code{var_by} option allow to compare different population or cancer.
	The \code{CI5_comparaison} option allow to compare with the CI5X and therefore test the quality of the data.
	If the population data stops before 85+ (75+ for instance), the population data \bold{must} be 0 when the population data is unknown so, the program can detect automatically the last age group (70+,75+,80+ or 85+) for population.

}
\value{
	Return a plot and a dataframe.
	
}
\references{
	\url{https://www.iarc.fr/en/publications/pdfs-online/epi/sp95/sp95-chap11.pdf}
	
}
\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
}

\examples{

	data(csu_registry_data_1)
	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population.
	test <- csu_registry_data_1[csu_registry_data_1$registry_label=="Colombia, Cali",]

	# plot age specific rate for 1 population.
	csu_ageSpecific(test,
                plot_title = "Colombia, Liver, male")
				
	# plot age specific rate for 1 population, and comparaison with CI5X data.
	csu_ageSpecific(test,
					plot_title = "Colombia, Liver, male",
					CI5_comparaison = "Liver")

	# plot age specific rate for 4 population, legend at the bottom and comparaison with CI5X data.
	csu_ageSpecific(csu_registry_data_1,
					var_by="registry_label",
					legend=csu_trend_legend(position="bottom", nrow = 2),
					plot_title = "Liver, male",
					CI5_comparaison = 7)
	
	# plot age specific rate for 4 population, legend at the right.
	csu_ageSpecific(csu_registry_data_1,
					var_by="registry_label",
					legend=csu_trend_legend(position="right", right_space_margin = 6.5),
					plot_title = "Liver, male")			  		  
}




