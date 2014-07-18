\name{ASTM.E313.Whiteness}
\alias{ASTM.E313.Whiteness}
\title{ASTM E313 Whiteness}
\description{\code{ASTM.E313.Whiteness} ASTM E313 Whiteness.
}
\usage{
ASTM.E313.Whiteness(XYZmatrix)
}
\arguments{\item{XYZmatrix}{ tri-stimulus values for the calculated for illuminant C}
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
XYZ<-c(0.1146538, 0.08391198, 0.08222077)
ASTM.E313.Whiteness(XYZ)
}
\keyword{datasets}

