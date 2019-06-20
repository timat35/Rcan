\name{csu_trendCohortPeriod}
\alias{csu_trendCohortPeriod}
\title{csu_trendCohortPeriod}

\description{
\code{csu_trendCohortPeriod}  plot cohort period age specific graph.
}


\usage{
csu_trendCohortPeriod(
  df_data,
  var_age = "age",
  var_cases="cases",
  var_py="py",
  var_year = "year",
  type = "Cohort",
  missing_age = NULL,
  logscale = TRUE,
  db_rate = 100000,
  first_age = 6,
  last_age = 16,
  year_group = 5,
  age_dropped=FALSE,
  plot_title = "csu_title",
  format_export = NULL,
  graph_dev =FALSE) 
}
  
\arguments{
  \item{df_data}{
  Data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_age}{
	Age group variable: (numerical) 18 5-years age-group, 0-4, 5-9, .. 85+ and an optional missing age.\cr
	The value can be 1,2..19, as well as 0,5,..85 or other value as long are they are numerical.
	}
  \item{var_cases}{
	Number of event (cases, deaths, ...) variable.
	}
  \item{var_py}{
	Population year variable.
	}
  \item{var_year}{
	Time variable.
	}
  \item{type}{
   Type of the plot:\cr
  	\tabular{ll}{
		"Cohort" \tab Cohort graph.\cr
		"Period" \tab Period graph.\cr
		"Both" \tab Cohort Period graph.\cr
		}
	}

  \item{missing_age}{
		Age value representing the missing age cases.
}
  \item{logscale}{
		Logical value: if \code{TRUE} Y-axis use logscale.
	}
  \item{db_rate}{
		The denominator population. Default is 100000.
}
  \item{first_age}{
		First age group included, \bold{must} be between 1 and 17. 1 represents 0-4, 2 represents 5-9, ... 5 represents 20-24 etc.\cr
		Default is 6 (25-29 years).
  }	
  \item{last_age}{
		Last age group included, \bold{must} be between 2 and 18. 2 represents 5-9, ... 5 represents 20-24, ... 18 represents 85+ etc.\cr
		Default is 16 (75-79 years).\cr
		To plot the age group from 15-19 years until 65-69 years, use: \code{first_age(4), last_age(14)}
  }
  \item{year_group}{
		Usually, data are regrouped in 5 years period. \cr.
		The numerical parameter controls the size of the group.
   }

  \item{age_dropped}{
		Only if some age grouped are missing in the data.
		Logical value: if \code{TRUE}, assume the age group not used are already dropped from the data.\cr
		First_age and last_age option \bold{must} still be filled.
}
  \item{plot_title}{
		Title of the plot.
	}
  \item{format_export}{
   export the graph in different format:\cr
  	\tabular{ll}{
		\code{NULL} \tab Plot in R studio windows.\cr
		\code{"pdf"} \tab Export in PDF format.\cr
		\code{"tiff"} \tab Export in TIFF 300dpi format.\cr
		\code{"png"} \tab Export in PNG 200dpi format.\cr
		\code{"svg"} \tab Export in SVG format. Can be edit with \url{https://inkscape.org/fr/}.\cr
		}
	The filename is the \code{plot_title} option.
	}
	\item{graph_dev}{
		If the plot is embedded in a graphics Device function (such as pdf()), the graph_dev option should be set to TRUE for the first graph to avoid a blank page.
	}


}

\details{
	This function is design the plot a the age-specific cohort and period plot. 
	The \code{type} option allow to choose between the 3 different graphics: "Cohort", "Period", or "Both".
	Please note than the cohort plot and the period plot can be superimposed if the \code{first_age} is too low.

}
\value{
	Return a plot.
	
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
	\code{\link{csu_bar_top}}
	\code{\link{csu_time_trend}}

}

\examples{

	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population 
	test <- subset(csu_registry_data_2,registry == 84020 & sex == 1)


	
	# plot cohort graph from 25-29 years until 75-79 years.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males")
						
	# plot Period graph from 0-5 until 85+.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Period",
						first_age=1,
						last_age=18)
						
	# plot Cohort-Period graph from 30-64 years until 70-74 years.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15)

	# plot Cohort-Period graph from 30-64 years until 70-74 years with Y axis normal scale.
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15,
						logscale=FALSE)

\donttest{					
	# plot Cohort graph from 25-29 years until 75-79 years, with data grouped in 2 years period.
	csu_trendCohortPeriod(df_data=test,
						  missing_age =99,
						  plot_title = "USA, Liver, males",
						  type="Cohort",
						  year_group = 2)
						  
	# Plot embedded in a graphic device
	pdf("example_test.pdf")
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15,
						graph_dev=TRUE)
			  
	csu_trendCohortPeriod(df_data=test,
                        missing_age =99,
                        plot_title = "USA, Liver, males",
						type="Both",
						first_age=7,
						last_age=15,
						logscale=FALSE)
				
	dev.off()
}		
}