\name{spectra2XYZ}
\alias{spectra2XYZ}
\title{convert spectral data to tristimulus values}
\description{\code{spectra2XYZ} convert spectral data to tristimulus values.
}
\usage{spectra2XYZ(spectraIn=NA, illuminantIn=NA, ciexyzIn=NA,wlIn=NA, wlInterval=NA) }
\arguments{
  \item{spectraIn}{ spectral data}
  \item{illuminantIn}{ illuminant}
  \item{ciexyzIn}{ range of illuminant wavelengths}
  \item{wlIn}{ range of spectral wavelengths}
  \item{wlInterval}{ arbitrary wavelength interval to be applied to all series through interpolation}
}
\value{
  XYZ coordinates
}
\source{
Andrew S. Glassner, 1995
Principles of digital image synthesis: Vol. 1
Kaufmann
}
\references{
Andrew S. Glassner, 1995
Principles of digital image synthesis: Vol. 1
Kaufmann
}
\author{Jose Gama}
\examples{
spectra2XYZ(MaterialReferenceData[,c('wavelength','BlueSky')])
}
\keyword{datasets}

