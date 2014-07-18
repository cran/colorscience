\name{xy2CCT.McCamy}
\alias{xy2CCT.McCamy}
\title{convert from chromaticity coordinates to correlated color temperature}
\description{\code{xy2CCT.McCamy}  convert from chromaticity coordinates to 
correlated color temperature (approximation).
}
\usage{xy2CCT.McCamy(x,y) }
\arguments{
  \item{x}{ x coordinates}
  \item{y}{ y coordinates}
}
\value{
  CCT McCamy
}
\source{
C. S. McCamy, 1992
"Correlated color temperature as an explicit function of chromaticity coordinates"
Color Research & Application Volume 17, Issue 2, pages 142–144
}
\references{
C. S. McCamy, 1992
"Correlated color temperature as an explicit function of chromaticity coordinates"
Color Research & Application Volume 17, Issue 2, pages 142–144
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
xy2CCT.McCamy(xyY[1],xyY[2])
}
\keyword{datasets}

