\name{Yxy2CIE1960UCS}
\alias{Yxy2CIE1960UCS}
\title{convert from Yxy coordinates to CIE 1960 UCS}
\description{\code{Yxy2CIE1960UCS} convert from Yxy coordinates to CIE 1960 UCS.
}
\usage{Yxy2CIE1960UCS(Yxymatrix) }
\arguments{
  \item{Yxymatrix}{ Yxy coordinates}
}
\value{
  CIE 1960 UCS
}
\source{
Bruce Justin Lindbloom, 2014
\url{http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_T.html}
}
\references{
Bruce Justin Lindbloom, 2014
\url{http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_T.html}
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
Yxy2CIE1960UCS(xyY[c(3,1,2)])
}
\keyword{datasets}

