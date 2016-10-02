\name{Berger59.Whiteness}
\alias{Berger59.Whiteness}
\title{Berger (59) Whiteness}
\description{\code{Berger59.Whiteness} formula was developed by 
A. Berger (formerly employee of Bayer AG, Germany and was presented in 1959.
}
\usage{Berger59.Whiteness(xyYmatrix, illuminant = "C", observer = 2, RefWhite =
                 get("XYZperfectreflectingdiffuser", envir = environment()))}
\arguments{
    \item{xyYmatrix}{ CIE values for illuminant C}
    \item{illuminant}{ illuminant}
    \item{observer}{ observer}
    \item{RefWhite}{ White Reference}
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
Berger59.Whiteness(xyY)
}
\keyword{datasets}

