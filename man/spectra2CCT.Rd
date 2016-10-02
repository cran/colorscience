\name{spectra2CCT}
\alias{spectra2CCT}
\title{Correlated Color Temperature (CCT) from spectra}
\description{\code{spectra2CCT} Correlated Color Temperature (CCT) from spectra.
}
\usage{spectra2CCT(SPD=NA, isoTempLinesTable=NA,
CIETable = get("ciexyz31", envir = environment()), TCS = get("TCSdata", 
envir = environment())) }
\arguments{
  \item{SPD}{ light source spd}
  \item{isoTempLinesTable}{ Iso temperature lines table}
  \item{CIETable}{ reference data values}
  \item{TCS}{ spectral reflectance data of 14 color test samples for CRI}
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
# illuminant A
SPD = illuminants[1:51*2-1,c('wlnm','A')] # every 10 nm
CCT <- spectra2CCT(SPD)
CCT
}
\keyword{datasets}

