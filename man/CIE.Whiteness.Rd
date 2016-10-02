\name{CIE.Whiteness}
\alias{CIE.Whiteness}
\title{CIE Whiteness}
\description{\code{CIE.Whiteness} The CIE Whiteness index is widely used
in the industry for D65 for 2 or 10 deg observer.
}
\usage{CIE.Whiteness(xyYmatrix, illuminant = "D65", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment()))}
\arguments{
    \item{xyYmatrix}{ xyY data}
    \item{illuminant}{ illuminant}
    \item{observer}{ observer}
    \item{RefWhite}{ Reference White}
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
CIE.Whiteness(xyY)
}
\keyword{datasets}

