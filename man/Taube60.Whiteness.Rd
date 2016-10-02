\name{Taube60.Whiteness}
\alias{Taube60.Whiteness}
\title{Taube Whiteness}
\description{\code{Taube60.Whiteness} developed by Mr. Taube (formerly an 
employee of BASF AG, Germany). It was presented in 1960 and has found it's 
application mainly in the plastic sector.
}
\usage{Taube60.Whiteness(XYZmatrix, illuminant = "D65", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment()))}
\arguments{
    \item{XYZmatrix}{ CIE tristimulus values for illuminant C}
    \item{illuminant}{ illuminant}
    \item{observer}{ observer}
    \item{RefWhite}{ White reference}
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
XYZ<-c(0.11465380, 0.08391198, 0.08222077)
Taube60.Whiteness(XYZ)
}
\keyword{datasets}

