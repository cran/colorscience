\name{emittanceblackbodyPlanck}
\alias{emittanceblackbodyPlanck}
\title{emittance of a black body of temperature T at a given wavelength}
\description{\code{emittanceblackbodyPlanck} emittance of a black body of 
temperature T at a given wavelength (in metres).}
\usage{emittanceblackbodyPlanck(wlnm, T) }
\arguments{
  \item{wlnm}{ wavelength in nm}
  \item{T}{ temperature in Kelvin}
}
\value{
  emittance
}
\source{
Planck's radiation law
\url{https://en.wikipedia.org/wiki/Planck\%27s_law}
}
\references{
Planck's radiation law
\url{https://en.wikipedia.org/wiki/Planck\%27s_law}
}
\author{Jose Gama}
\examples{
emittanceblackbodyPlanck(555,2000)
}
\keyword{datasets}

