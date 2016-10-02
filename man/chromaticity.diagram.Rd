\name{chromaticity.diagram}
\alias{chromaticity.diagram}
\title{Plot the chromaticity diagram}
\description{\code{chromaticity.diagram} Plots the chromaticity diagram AKA 
 "horse shoe".
}
\usage{chromaticity.diagram(chromaticityCoordinates=get("cccie31", envir = environment()), 
conversionFunction=NULL,...) }
\arguments{
  \item{chromaticityCoordinates}{ chromaticity coordinates}
  \item{conversionFunction}{ optional function to perform the coordinate 
  conversion}
  \item{...}{ optional parameters for the plot command}
}
\value{
  none
}
\author{Jose Gama}
\examples{
chromaticity.diagram()
xl<-yl<-0:1
chromaticity.diagram(xlim=xl,ylim=yl)
chromaticity.diagram(conversionFunction=CIE1931XYZ2CIE1976uv, xlim=xl,ylim=yl,
xlab="u'",ylab="v'")
}
\keyword{datasets}

