\name{daylightcomponents}
\alias{daylightcomponents}
\title{daylight components}
\description{\code{daylightcomponents} table with the mean relative spectral 
radiant power distribution and first two eigenvectors for the CIE method of 
calculating daylight.}
\format{
  This data frame contains the following columns:
  \describe{
    \item{wlnm}{ wavelength in nm}
    \item{S0}{ mean relative spectral radiant power distribution}
    \item{S1}{ first eigenvector}
    \item{S2}{ second eigenvector}
  }
}
\source{
Wyszecki, G. and Stiles, W.S., 1982
Color Science: Concepts and Methods, Quantitative data and formulae.
John Wiley & Sons.
}
\references{
Wyszecki, G. and Stiles, W.S., 1982
Color Science: Concepts and Methods, Quantitative data and formulae.
John Wiley & Sons.
}
\author{Jose Gama}
\examples{
data(daylightcomponents)
str(daylightcomponents)
}
\keyword{datasets}

