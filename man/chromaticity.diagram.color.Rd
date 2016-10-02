\name{chromaticity.diagram.color}
\alias{chromaticity.diagram.color}
\alias{chromaticity.diagram.color.fill}
\title{Plot the chromaticity diagram line with color}
\description{\code{chromaticity.diagram.color} Plots the chromaticity diagram 
	AKA "horse shoe", as a black line, a color line or a polygon.
}
\usage{chromaticity.diagram.color(chromaticityCoordinates=get("cccie31", envir = environment()), 
conversionFunction=NULL, granularity=10, ...) }
\arguments{
  \item{chromaticityCoordinates}{ chromaticity coordinates}
  \item{conversionFunction}{ optional function to perform the coordinate 
  conversion}
  \item{granularity}{ granularity = number of calculated points}
  \item{...}{ optional parameters for the plot command}
}
\value{
  none
}
\author{Jose Gama}
\examples{
chromaticity.diagram.color()
xl<-yl<-0:1
chromaticity.diagram.color(xlim=xl,ylim=yl)
chromaticity.diagram.color(conversionFunction=CIE1931XYZ2CIE1976uv, xlim=xl,
ylim=yl,xlab="u'",ylab="v'")
}
\keyword{datasets}

