\name{conversionLuminance}
\alias{conversionLuminance}
\title{Conversion Factors for Units of Luminance}
\usage{conversionLuminance}
\description{\code{conversionLuminance} is a table of conversion factors for units of Luminance}
\format{
  This data frame contains the following columns:
  \describe{
    \item{cd.m.2}{ cd/m^2 = nit}
    \item{cd.cm.2}{ cd/cm^2 = stilb}
    \item{cd.ft.2}{ cd/ft^2}
    \item{cd.in.2}{ cd/in^2}
    \item{apostilb}{ apostilb = blondel}
    \item{millilambert}{ millilambert}
    \item{footlambert}{ foot-lambert}
  }
}
\source{
J. Meyer-Arendt, "Radiometry and Photometry: Units and Conversion Factors," Appl. Opt.  7, 2081-2081 (1968).
}
\references{
J. Meyer-Arendt, "Radiometry and Photometry: Units and Conversion Factors," Appl. Opt.  7, 2081-2081 (1968).
}
\author{Jose Gama}
\examples{
data(conversionLuminance)
conversionLuminance
}
\keyword{datasets}

