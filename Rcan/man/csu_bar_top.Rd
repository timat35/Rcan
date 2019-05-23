\name{csu_bar_top}
\alias{csu_bar_top}
\title{csu_bar_top}

\description{
\code{csu_bar_top} Plot top X single-sided or double-sided bar chart.
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
  Data (need to be R \code{data.frame} format, see example to import csv file).
	}
  \item{var_value}{
	Value variable.\cr
	There must be only 1 value for each bar (\code{var_bar}).
	}
  \item{var_bar}{
	Bar label variable.
	}
  \item{group_by}{
  	\itemize{
  		\item Single-sided bar chart. \code{NULL} (default)
  		\item Double-sided bar chart. Variable name with one 2 values. (sex, ...).
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
	x-axes title. (For example, "Number of cases").
	}
	\item{label_by}{
	2 values vector. Will overwrite the legend label for double-sided bar chart. See (\code{group_by}).\cr
	For example: c("Male", "Female"). 
	}
  \item{color}{
  	code are hexadecimal.\cr
	\itemize{
  		\item Single-sided bar chart. 1 hexadecimal color code (same color for each bar) or variable name with a color associate to each bar label variable. 
  		\item Double-sided bar chart. 2 values vector. For example: c("#2c7bb6","#b62ca1").
  	}
	}
	\item{digits}{
	Number of decimal digits. Default: 1
	}
 
}
\details{
	This function plot a top X bar chart, single-sided or double sided.
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

library(Rcan)
data("csu_CI5X_data")

#get the registry code asssociate to registry_label
print(unique(csu_CI5X_data[,c("registry_label", "registry_code")]),nrows = 1000)
	
}




