\name{spectra2ISObrightness}
\alias{spectra2ISObrightness}
\title{Diffuse blue reflectance factor (ISO brightness)}
\description{\code{spectra2ISObrightness} Diffuse blue reflectance factor 
(ISO brightness), R457,  ISO 2470.
}
\usage{spectra2ISObrightness(spectraIn=NA, wlIn=NA, 
RSDmatrix=get("ISObrightnessReflectometerRSD", envir = environment())) }
\arguments{
  \item{spectraIn}{ spectral data}
  \item{wlIn}{ wavelength range}
  \item{RSDmatrix}{ ISO brightness data}
}
\value{
  LCHuv coordinates
}
\source{
ISO board, 2009
ISO 2470-1 : 2009 PAPER, BOARD AND PULPS
MEASUREMENT OF DIFFUSE BLUE REFLECTANCE FACTOR
PART 1 INDOOR DAYLIGHT CONDITIONS (ISO BRIGHTNESS)
}
\references{
ISO board, 2009
ISO 2470-1 : 2009 PAPER, BOARD AND PULPS
MEASUREMENT OF DIFFUSE BLUE REFLECTANCE FACTOR
PART 1 INDOOR DAYLIGHT CONDITIONS (ISO BRIGHTNESS)
}
\author{Jose Gama}
\examples{
spectra2ISObrightness(MaterialReferenceData[,c( 'BlueSky')], 
MaterialReferenceData[,c('wavelength' )])
}
\keyword{datasets}

