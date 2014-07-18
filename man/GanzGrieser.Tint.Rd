\name{GanzGrieser.Tint}
\alias{GanzGrieser.Tint}
\title{Ganz and Grieser Tint}
\description{\code{GanzGrieser.Tint} Ganz Grieser Tint Method.
}
\usage{GanzGrieser.Tint(xyYmatrix)}
\arguments{
    \item{xyYmatrix}{ CIE xyY values for illuminant C}
}
\source{
Xrite, 2012
Color iQC and Color iMatch Color Calculations Guide
Version 8.0
30 July 2012 Revision 1.0
}
\references{
Xrite, 2012
Color iQC and Color iMatch Color Calculations Guide
Version 8.0
30 July 2012 Revision 1.0
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
GanzGrieser.Tint(xyY)
}
\keyword{datasets}

