\name{spectra2CRIGAIFSCI}
\alias{spectra2CRIGAIFSCI}
\title{CRI, GAI and FSCI from spectra}
\description{\code{spectra2CRIGAIFSCI} Color Rendering Index (CRI), Gamut Area 
Index (GAI) and full spectrum index (FSCI) from spectra.
}
\usage{spectra2CRIGAIFSCI(SPD=NA, isoTempLinesTable=NA, CCT=NA, 
CIETable = get("ciexyz31", envir = environment()), TCS = get("TCSdata", 
envir = environment())) }
\arguments{
  \item{SPD}{ light source spd}
  \item{isoTempLinesTable}{ Iso temperature lines table}
  \item{CCT}{ Correlated Color Temperature (CCT)}
  \item{CIETable}{ reference data values}
  \item{TCS}{ spectral reflectance data of 14 color test samples for CRI}
}
\value{
  CRI, GAI and FSCI
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
\dontrun{
SPD = illuminants[1:51*2-1,c('wlnm','A')] # every 10 nm
isoTempLinesTable <- createIsoTempLinesTable(SPD)
CCT <- spectra2CCT(SPD)
spectra2CRIGAIFSCI(SPD, isoTempLinesTable, CCT)
spectra2CRIGAIFSCI(SPD, isoTempLinesTable)
spectra2CRIGAIFSCI(SPD)
}
}
\keyword{datasets}

