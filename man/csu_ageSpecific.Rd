\name{csu_ageSpecific}
\alias{csu_ageSpecific}
\title{csu_ageSpecific}

\description{
\code{csu_ageSpecific} calculate and plot Age-Specific Rate.
}
\usage{
csu_ageSpecific(df_data,
		var_age="age",
        var_cases="cases",
        var_py="py",
        group_by = NULL, 
        missing_age = NULL,
        db_rate = 100000,
		logscale=FALSE,
        plot_title=NULL,
        legend=csu_trend_legend(),
        color_trend = NULL,
        CI5_comparison=NULL,
        var_rate="rate") 
}
  
\arguments{
  \item{df_data}{
  Data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_age}{
		Age variable. Several format are accepted 
		\tabular{lll}{
			1 \tab "0-4" \tab 0 \cr
			2 \tab "5-9" \tab 5 \cr
			3 \tab "10-14" \tab 10 \cr
			... \tab ... \tab ... \cr
			17 \tab "80-84" \tab 80 \cr
			18 \tab "85+" \tab 85 \cr
		}
		Missing age value must be precise in the option \code{missing_age}.\cr
		Last age group will always be considere without size (ie: 80+,85+, etc..). 
	}
  \item{var_cases}{
	Number of event (cases, deaths, ...) variable.
	}
  \item{var_py}{
	Population year variable.
	}
  \item{group_by}{
	Variable to compare different age specific rate (sex, country, cancer ...).\cr
	Only one variable can be chosen.
	}
  \item{missing_age}{
		Age value representing the missing age cases.
	}
  \item{db_rate}{
	The denominator population. Default is 100000.
	}
  \item{logscale}{
		Logical value: if \code{TRUE} Y-axis use logscale.
	}
  \item{plot_title}{
	Title of the plot.
	}
  \item{legend}{
	Legend option: see \code{\link{csu_trend_legend}}.\cr
	Use only if \code{group_by} is not \code{NULL}.
	}
  \item{color_trend}{
	Vector of color for the trend.
	The color codes are hexadecimal (e.g. "#FF0000") or predefined R color names (e.g. "red").
	}
  \item{CI5_comparison}{
	Add a dotted line representing the CI5XII for a specific cancer.\cr
	Value can be a cancer_label (example: "Liver"), or a cancer_code (example = 16).\cr
	See \code{\link{csu_ci5_mean}} to get the list of possible values.
	}

  \item{var_rate}{
	Name of the age specific variable if a dataframe is return.
	}


}
\details{
	This function calculate and plot the age specific rate. 
	The \code{group_by} option allow to compare different population or cancer.
	The \code{CI5_comparison} option allow to compare with the CI5XII and therefore test the quality of the data.
	If the population data stops before 85+ (75+ for instance), the population data \bold{must} be 0 when the population data is unknown so, the program can detect automatically the last age group (70+,75+,80+ or 85+) for population.

}
\value{
	Return a plot and a \code{data.frame}.
	
}
\references{
	\url{https://publications.iarc.fr/_publications/media/download/3753/609d0d7711047dd76d7f3dbaa25d7f041fcd013e.pdf}
	
}
\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_group_cases}}
	\code{\link{csu_merge_cases_pop}}
	\code{\link{csu_asr}}
	\code{\link{csu_cumrisk}}
	\code{\link{csu_eapc}}
	\code{\link{csu_ageSpecific_top}}
	\code{\link{csu_bar_top}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
}

\examples{

data(csu_registry_data_1)
data(csu_registry_data_2)

# you can import your data from csv file using read.csv:
# mydata <-  read.csv("mydata.csv", sep=",")

# to select only 1 population.
test <- subset(csu_registry_data_1 , registry_label == "Colombia, Cali")


# plot age specific rate for 1 population.
csu_ageSpecific(test,
				plot_title = "Colombia, Liver, male")

# plot age specific rate for 1 population, and comparison with CI5XII data.
csu_ageSpecific(test,
				plot_title = "Colombia, Liver, male",
				CI5_comparison = "Liver")

# plot age specific rate for 4 population, 
# legend at the bottom and comparison with CI5XII data using cancer code.
csu_ageSpecific(
	csu_registry_data_1,
	group_by="registry_label",
	legend=csu_trend_legend(position="bottom", nrow = 1),
	plot_title = "Liver, male",
	CI5_comparison = 16
	)

	
\donttest{
# plot age specific rate for 4 population, legend at the right.
csu_ageSpecific(
	csu_registry_data_1,
	group_by="registry_label",
	legend=csu_trend_legend(
		position="right", right_space_margin = 6.5
	),
	plot_title = "Liver, male")	



# Plot embedded in a graphic device
pdf(paste0(tempdir(),"/test.pdf"),width = 11.692 , height =  8.267) 


csu_ageSpecific(
	csu_registry_data_1,
	group_by="registry_label",
	legend=csu_trend_legend(position="bottom", nrow = 2),
	plot_title = "Liver, male",
	CI5_comparison = 16)

plot.new()

csu_ageSpecific(
	csu_registry_data_1,
	group_by="registry_label",
	legend=csu_trend_legend(
		position="right", right_space_margin = 6.5
	),
	plot_title = "Liver, male")	



dev.off()

}



	
}




