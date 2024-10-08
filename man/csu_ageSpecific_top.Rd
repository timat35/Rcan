\name{csu_ageSpecific_top}
\alias{csu_ageSpecific_top}
\title{csu_ageSpecific_top}

\description{
\code{csu_ageSpecific_top} calculate and plot Age-Specific Rate for the top X cancer or top X population.
}
\usage{
csu_ageSpecific_top(df_data,
           var_age="age",
       	   var_cases="cases",
           var_py="py",
           var_top, 
           group_by=NULL,
           missing_age=NULL,
           db_rate = 100000,
           logscale = FALSE,
           nb_top = 5,
           plot_title=NULL,
           plot_subtitle=NULL,
           var_color=NULL)
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
  \item{var_top}{
	Cancer label variable or country/registry variable for example.
	}
  \item{group_by}{
	Variable to compare different age specific rate (sex, registry ...).\cr
	Only one variable can be chosen.
	One graph will be produce per variable.
	If the variable is a factor, the labels associate to the variable will be the subtitle of the plot.
	See \code{\link{factor}}.
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
   \item{nb_top}{
		Lowest Rank include. Default is 5.
	}
  \item{plot_title}{
	Title of the plot.
	}
  \item{plot_subtitle}{
	Subtitle of the plot. (For example, "Top 5 cancer").
	}
  \item{var_color}{
	Variable with a color associate to each cancer.\cr
	The color codes are hexadecimal (e.g. "#FF0000") or predefined R color names (e.g. "red").\cr
	This allow to keep the same color coding for each graph.
	}
 
}
\details{
	This function keep only the top X cancer and plot their age specific rate.
	The \code{group_by} option allow to compare different population. 
	If the population data stops before 85+ (75+ for instance), the population data \bold{must} be 0 when the population data is unknown so, the program can detect automatically the last age group (70+,75+,80+ or 85+) for population.

}
\value{
	Return plots and a \code{data.frame}.
	
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
	\code{\link{csu_ageSpecific}}
	\code{\link{csu_bar_top}}
	\code{\link{csu_time_trend}}
	\code{\link{csu_trendCohortPeriod}}
}

\examples{

library(Rcan)
data("csu_CI5XII_data")

#get the id_code asssociate to id_label
#print(unique(csu_CI5XII_data[,c("id_label", "id_code")]),nrows = 1000)

#get the cancer code asssociate to cancer_label
#print(unique(csu_CI5XII_data[,c("cancer_label", "cancer_code")]),nrows = 1000)

#remove all cancers:
df_data <- subset(csu_CI5XII_data ,cancer_code < 62)
df_data$sex <- factor(df_data$sex, levels=c(1,2), labels=c("Male", "Female"))

#select Thailand changmai
df_data_1 <- subset(df_data, id_code==476400199)


# plot for Thailand Changmai
dt_result_1 <- 
	csu_ageSpecific_top(df_data_1,
		var_age="age", 
		var_cases="cases", 
		var_py="py",
		var_top="cancer_label", 
		group_by="sex", 
		plot_title= "Thailand, Chiangmai",
		plot_subtitle = "Top 5 cancer",
		missing_age = 19)

\donttest{

	#select USAm NPCR
	df_data_2 <- subset(df_data,id_code== 384008099)


	# plot for USA NPCR
	dt_result_2 <- 
		csu_ageSpecific_top(
			df_data_2,
			var_age="age", 
			var_cases="cases", 
			var_py="py",
			var_top="cancer_label", 
			group_by="sex", 
			plot_title= "USA",
			plot_subtitle = "Top 5 cancer",
			missing_age = 19
			)
}



	
}




