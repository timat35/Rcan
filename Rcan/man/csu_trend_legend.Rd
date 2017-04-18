\name{csu_trend_legend}
\alias{csu_trend_legend}
\title{csu_trend_legend}

\description{
\code{csu_trend_legend} legend option use in Rcan package for trends.
}
\usage{
	csu_trend_legend(title=NULL, position="bottom",nrow=1, right_space_margin=1)
}

\arguments{
  \item{title}{
   Title of the legend.
   }
   \item{position}{
   Position of the legend:\cr
	\tabular{ll}{
		"bottom" \tab (default) The legend is place at the bottom of the graph. use with option \code{nrow}.\cr
		"right" \tab (default)  The Legend is place at the right of each trend. use with option \code{right_space_margin}.\cr
		}
	}
	\item{nrow}{
		Number of row of the legend (only of position = "bottom").
	}
	\item{right_space_margin}{
		If the legend position is "right", the right margin need to be enough big, to contain the text legend.
		value should be between 1 and 10 depending the size of the text legend.
	}

}
\details{
		This function return a structure which is only use in the trend graphics of the package \code{Rcan}

}
\value{
	Return a structured list.
}

\author{
	Mathieu Laversanne
}

\seealso{
	\code{\link{csu_ageSpecific}}
}

\examples{

data(csu_registry_data_1)

csu_ageSpecific(csu_registry_data_1,
                var_by="registry_label",
                legend=csu_trend_legend(title="registry", position="bottom", nrow = 2),
                plot_title = "Legend: bottom")
				
csu_ageSpecific(csu_registry_data_1,
                var_by="registry_label",
                legend=csu_trend_legend(position="right", right_space_margin = 2),
                plot_title = "Legend: right, cut")

csu_ageSpecific(csu_registry_data_1,
                var_by="registry_label",
                legend=csu_trend_legend(position="right", right_space_margin = 6.5),
                plot_title = "Legend: right")
				
#See more examples here:
help(csu_ageSpecific)
	  		  
}




