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
		group_by = NULL,
		logscale = TRUE,
		smoothing = NULL,
		legend = csu_trend_legend(),
		yaxes_title = "Age standardized rate per 100000",
		plot_title = "csu_title",
		format_export = NULL,
		graph_dev =FALSE) 
}
  
\arguments{
  \item{df_data}{
  Data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_trend}{
	Statistics variable to be plot on Y axis.\cr
	Usually for the ASR, but it could be the cumulative risk, or even the number of cases.
	}
  \item{var_year}{
	Time variable.
	}
  \item{group_by}{
		Variable to compare different age specific rate (sex, country, cancer ...).\cr
		Only one variable can be chosen.
	}
  \item{logscale}{
		Logical value: if \code{TRUE} Y-axis use logscale.
	}
  \item{smoothing}{
		Apply a smoothing using the R loess function. \cr.
		The numerical parameter controls the degree of smoothing.\cr
		See option \code{span} in \code{\link{loess}}.
	}
  \item{legend}{
		legend option: see \code{\link{csu_trend_legend}}.\cr
		Use only if \code{group_by} is not \code{NULL}.
	}
  \item{yaxes_title}{
		Y-axis title. To choose in fonction for the \code{var_trend} option.
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
	This function is design the plot a statistics over time. It has been design for the ASR by year, but can be used for other statistics over time period.
	The \code{group_by} option allow to compare different population or cancer.

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

	data(csu_registry_data_2)

	# you can import your data from csv file using read.csv:
	# mydata <-  read.csv("mydata.csv", sep=",")
	
	# to select only 1 population 
	test <- csu_registry_data_2[csu_registry_data_2$registry_label=="Colombia, Cali",]
	
	# to change sex variable to factor with label
	test$sex <- factor(test$sex, levels=c(1,2), labels=c("Male", "Female"))
	
	# to calculate the asr
	df_asr <- csu_asr(test,missing_age = 99,
                  group_by  = c("registry", "registry_label", "year", "sex"),
                  var_age_group =  c("registry", "registry_label"))
	
	
	# plot ASR ove year, by sex.
	csu_trend(df_asr, group_by="sex",
                plot_title = "Colombia, Liver")
				
	# plot ASR over year, by sex, with small smoothing.
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.3)
			  
	# plot ASR over year, by sex, with high smoothing.
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.5)
			  
	# Plot embedded in a graphic device
	pdf("example_test.pdf")
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.3,
			  graph_dev=TRUE)
			  
	csu_trend(df_asr, group_by="sex",
			  plot_title = "Colombia, Liver",
			  smoothing = 0.5)
				
	dev.off()
				
}