\name{emittanceblackbodyPlanck}
\alias{emittanceblackbodyPlanck}
\title{emittance of a black body of temperature T at a given wavelength}
\description{\code{emittanceblackbodyPlanck} emittance of a black body of temperature T at a given wavelength (in metres).
}
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
\url{http://www.fourmich/documents/specrend/specrend.c}
}
\references{
Planck's radiation law
\url{http://www.fourmich/documents/specrend/specrend.c}
}
\author{Jose Gama}
\examples{
emittanceblackbodyPlanck(555,2000)
}
\keyword{datasets}

