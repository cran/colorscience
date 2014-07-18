\name{ASTM.E313.YellownessIndex}
\alias{ASTM.E313.YellownessIndex}
\title{ASTM E313 Yellowness}
\description{\code{ASTM.E313.YellownessIndex} ASTM E313 has successfully been 
used for a variety of white or near white materials.
}
\usage{ASTM.E313.YellownessIndex(XYZmatrix)}
\arguments{
    \item{XYZmatrix}{ tri-stimulus values for the calculated for illuminant C}
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
ASTM.E313.YellownessIndex(XYZ)
}
\keyword{datasets}

