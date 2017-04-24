\name{csu_trend}
\alias{csu_trend}
\title{csu_trend}

\description{
\code{csu_trend}  plot stats over year.
}


\usage{
csu_trend(df_data,
		var_trend = "asr",
		var_year = "year",
		var_by = NULL,
		logscale = TRUE,
		smoothing = 0.3,
		legend = csu_trend_legend(),
		plot_title = "csu_title",
		yaxes_title = "Age standardized (world) rate per 100,000",
		format_export = NULL)
}
  
\arguments{
  \item{df_data}{
  Data (need to be R dataframe format, see example to import csv file).
	}
  \item{var_trend}{
	(Optional, String) Statistics variable to be plot on Y axis.\cr
	Usually for the ASR, but it coud be the cumulative risk, or even the number of cases.\cr
	Default is "asr".
	}
  \item{var_year}{
		(Optional, String) Time variable (incidence).\cr
		Default is "year".
	}
  \item{var_by}{
		(Optional, String) Variable to compare across different group (sex, country, cancer ...).\cr
		Only one variable can be choose.
	}
  \item{logscale}{
		(Optional, Boolean) Y-axis scale (log or normal). Default is TRUE.
	}
  \item{smoothing}{
		(Optional, numeric) Apply a smoothing using the R loess function. \cr.
		The parameter control the "force of the smooth". Default is 0.3.
	}
  \item{legend}{
		(Optional) legend option: see \code{\link{csu_trend_legend}}.\cr
		Use only if \code{var_by} not NULL
	}
  \item{plot_title}{
		(Optional, String) Title of the plot. Default is csu_title.
	}

  \item{yaxes_title}{
		(Optional, string) Y-axis title. To choose in fonction for the \code{var_trend} option.\cr
		Default is "Age standardized (world) rate per 100,000".
	}
  \item{format_export}{
		(Optional) export format 4 option: "pdf", "png", "tiff", "svg". The filename is the \code{plot_title} option.
	}

}

\details{
	This function is design the plot a statistics over time. It has been design for the ASR by year, but can bve used for other statistics over time period.
	The \code{var_by} option allow to compare different population or cancer.

}
\value{
	Return a plot.
	
}

\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_asr}}
	\code{\link{csu_eapc}}
	\code{\link{csu_trendCohortPeriod}}
}

\examples{

	data(csu_registry_data_1)
	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population 
	test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]
	
	# to change sex variable to factor with label
	test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))
	
	# to calculate the asr
	df_asr <- csu_asr(test,missing_age = 99,
                  var_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))
	
	
	# plot ASR ove year, by sex.
	csu_trend(df_asr, var_by="sex",
                plot_title = "Colombia, Liver, male")
				
}