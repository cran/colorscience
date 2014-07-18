\name{xy2CCT.HernandezAndres}
\alias{xy2CCT.HernandezAndres}
\title{convert from chromaticity coordinates to correlated color temperature 
(Hernandez Andres)}
\description{\code{xy2CCT.HernandezAndres}  convert from chromaticity 
coordinates to correlated color temperature (approximation) 
by Hernandez Andres.
}
\usage{xy2CCT.HernandezAndres(x,y) }
\arguments{
  \item{x}{ x coordinates}
  \item{y}{ y coordinates}
}
\value{
  CCT (Hernandez Andres)
}
\source{
Hernandez-Andres, et al. 1999
"Calculating correlated color temperatures across the entire gamut of daylight 
and skylight chromaticities"
\url{http://en.wikipedia.org/wiki/Color_temperature}
}
\references{
Hernandez-Andres, et al. 1999
"Calculating correlated color temperatures across the entire gamut of daylight 
and skylight chromaticities"
\url{http://en.wikipedia.org/wiki/Color_temperature}
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
xy2CCT.HernandezAndres(xyY[1],xyY[2])
}
\keyword{datasets}

