\name{Maxwell.triangle}
\alias{Maxwell.triangle}
\alias{Maxwell.triangle.color}
\alias{Maxwell.triangle.color.fill}
\title{Plot the Maxwell triangle}
\description{\code{Maxwell.triangle} Plots the Maxwell triangle, as a black 
line, a color line or a polygon.
}
\usage{Maxwell.triangle(primariesRGB=get("whitepointsRGB", envir = environment()), 
conversionFunction=NULL,...) }
\arguments{
  \item{primariesRGB}{ primarie valuess for RGB color spaces}
  \item{conversionFunction}{ optional function to perform 
the coordinate conversion}
  \item{...}{ optional parameters for the plot command}
}
\value{
  none
}
\author{Jose Gama}
\examples{
Maxwell.triangle()
xl<-yl<-0:1
Maxwell.triangle(xlim=xl,ylim=yl)
Maxwell.triangle(conversionFunction=CIE1931XYZ2CIE1976uv, 
xlim=xl,ylim=yl,xlab="u'",ylab="v'")
}
\keyword{datasets}

