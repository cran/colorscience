\name{DominantWavelength}
\alias{DominantWavelength}
\title{Converts xyY coordinates to wavelength}
\description{\code{DominantWavelength} Converts xyY coordinates to wavelength.
}
\usage{DominantWavelength(xyYmatrix, illuminant='D65',observer=2,
RefWhiteIllum=get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{xyYmatrix}{ xyY matrix}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhiteIllum}{ Reference White}
}
\value{
  Dominant Wavelength
}
\source{
Bruce Justin Lindbloom, 2013 
\url{http://www.brucelindbloom.com/index.html?ColorCalculator.html}
}
\references{
Bruce Justin Lindbloom, 2013 
\url{http://www.brucelindbloom.com/index.html?ColorCalculator.html}
}
\author{Jose Gama}
\examples{
DominantWavelength(c(0.59072, 0.85570, 0.80283))
}
\keyword{datasets}

