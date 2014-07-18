\name{CIETint}
\alias{CIETint}
\title{Tint indices: CIE Tint and ASTM E313 Tint}
\description{\code{CIETint} Tint indices: CIE Tint and ASTM E313 Tint.
}
\usage{CIETint(xymatrix,illuminant='D65',observer=2) }
\arguments{
  \item{xymatrix}{ matrix with xy data}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
}
\value{
  Tint
}
\source{
CIE, 2004
CIE Publication 15:2004, "Colorimetry"
ASTM E313, "Standard Practice for Calculating Yellowness and Whiteness Indices from Instrumentally Measured Color Coordinates"
}
\references{
CIE, 2004
CIE Publication 15:2004, "Colorimetry"
ASTM E313, "Standard Practice for Calculating Yellowness and Whiteness Indices from Instrumentally Measured Color Coordinates"
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
CIETint(xyY)
}
\keyword{datasets}

