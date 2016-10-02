\name{DIN6167.YellownessIndex}
\alias{DIN6167.YellownessIndex}
\title{CIE Whiteness}
\description{\code{DIN6167.YellownessIndex} The CIE Whiteness index is widely used
in the industry for D65 for 2 or 10 deg observer.
}
\usage{DIN6167.YellownessIndex(XYZmatrix, illuminant = "C", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment()))}
\arguments{
    \item{XYZmatrix}{ CIE values for illuminant C}
    \item{illuminant}{ illuminant}
    \item{observer}{ observer}
    \item{RefWhite}{ Reference White}
}
\source{
Scandinavian Pulp, paper and board, 2003
Basic equations for optical properties
SCAN-G 5:03 Revised 2003
}
\references{
Scandinavian Pulp, paper and board, 2003
Basic equations for optical properties
SCAN-G 5:03 Revised 2003
}
\author{Jose Gama}
\examples{
XYZ<-c(0.11465380, 0.08391198, 0.08222077)
DIN6167.YellownessIndex(XYZ)
}
\keyword{datasets}

