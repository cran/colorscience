\name{spectra2lux}
\alias{spectra2lux}
\title{Illuminance (Lux) from spectra}
\description{\code{spectra2lux} Illuminance (Lux) from spectra.
}
\usage{spectra2lux(spectraIn=NA, ciexyzIn=NA,wlIn=NA, wlInterval=NA) }
\arguments{
  \item{spectraIn}{ light source spd}
  \item{ciexyzIn}{ reference data values}
  \item{wlIn}{ range of output wavelengths}
  \item{wlInterval}{ arbitrary wavelength interval to be applied to all series through interpolation}
}
\value{
  Correlated Color Temperature (CCT)
}
\source{
Rensselaer Polytechnic Institute
Light Sources and Color Q & A
Appendix B: MATLAB script for calculating measures of light source color: CCT, CRI, GA, and FSI
\url{http://www.lrc.rpi.edu/programs/nlpip/lightinganswers/lightsources/appendixb1.asp}
}
\references{
Rensselaer Polytechnic Institute
Light Sources and Color Q & A
Appendix B: MATLAB script for calculating measures of light source color: CCT, CRI, GA, and FSI
\url{http://www.lrc.rpi.edu/programs/nlpip/lightinganswers/lightsources/appendixb1.asp}
}
\author{Jose Gama}
\examples{
spectra2lux(MaterialReferenceData[,c('wavelength','BlueSky')])
}
\keyword{datasets}

