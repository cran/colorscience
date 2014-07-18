\name{heuristic.wlnm2RGB}
\alias{heuristic.wlnm2RGB}
\title{Approximations from wavelengths to RGB}
\description{\code{heuristic.wlnm2RGB} Approximations from wavelengths to RGB.
}
\usage{heuristic.wlnm2RGB(wavelength, Gamma = 0.8, IntensityMax = 1) }
\arguments{
  \item{wavelength}{ wavelenght data}
  \item{Gamma}{ Gamma}
  \item{IntensityMax}{ maximum intensity}
}
\value{
  XYZ coordinates
}
\source{
Dan Bruton's, 2004
\url{www.physics.sfasu.edu/astro/color.html}
Earl F. Glynn 2006
Delphi conversion
\url{http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm}
}
\references{
Dan Bruton's, 2004
\url{www.physics.sfasu.edu/astro/color.html}
Earl F. Glynn 2006
Delphi conversion
\url{http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm}
}
\author{Jose Gama}
\examples{
heuristic.wlnm2RGB(555)
}
\keyword{datasets}

