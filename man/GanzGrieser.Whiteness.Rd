\name{GanzGrieser.Whiteness}
\alias{GanzGrieser.Whiteness}
\title{Ganz and Grieser Whiteness}
\description{\code{GanzGrieser.Whiteness} Dr. E. Ganz (formerly employee of Ciba
AG, Switzerland) and Mr.R. Griesser (formerly employee of J.R.Geigy) 
developed the Ganz Grieser Whiteness Method.
}
\usage{GanzGrieser.Whiteness(xyYmatrix)}
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
GanzGrieser.Whiteness(xyY)
}
\keyword{datasets}

