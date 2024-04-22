\name{createIsoTempLinesTable}
\alias{createIsoTempLinesTable}
\title{table of isotemperature lines for use with the Robertson Method}
\description{\code{createIsoTempLinesTable} table of isotemperature lines for 
use with the Robertson Method (Robertson, 1968) to interpolate isotemperature 
lines from the CIE 1960 UCS.
}
\usage{createIsoTempLinesTable(SPD=NA,CIETable = get("ciexyz31", envir = environment()), 
TCS = get("TCSdata", envir = environment())) }
\arguments{
  \item{SPD}{ light source spd}
  \item{CIETable}{ reference data values}
  \item{TCS}{ spectral reflectance data of 14 color test samples for CRI}
}
\value{
  Iso temperature lines table
}
\source{
Rensselaer Polytechnic Institute
Light Sources and Color Q & A
Appendix B: MATLAB script for calculating measures of light source color: CCT, CRI, GA, and FSI
\url{https://www.lrc.rpi.edu/programs/nlpip/lightinganswers/pdf/print/lightsources.pdf}
}
\references{
Rensselaer Polytechnic Institute
Light Sources and Color Q & A
Appendix B: MATLAB script for calculating measures of light source color: CCT, CRI, GA, and FSI
\url{https://www.lrc.rpi.edu/programs/nlpip/lightinganswers/pdf/print/lightsources.pdf}
}
\author{Jose Gama}
\examples{
# illuminant A
SPD = illuminants[1:51*2-1,c('wlnm','A')] # every 10 nm
isoTempLinesTable <- createIsoTempLinesTable(SPD)
}
\keyword{datasets}

