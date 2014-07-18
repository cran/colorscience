\name{ASTM.D1925.YellownessIndex}
\alias{ASTM.D1925.YellownessIndex}
\title{ASTM D 1925 Yellowness Index for Plastics}
\description{\code{ASTM.D1925.YellownessIndex} was developed for the definition
of the Yellowness of homogeneous, non-fluorescent, almost neutral-transparent,
white-scattering or opaque plastics as they will be reviewed under daylight 
condition.
}
\usage{ASTM.D1925.YellownessIndex(XYZmatrix)}
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
ASTM.D1925.YellownessIndex(XYZ)
}
\keyword{datasets}

