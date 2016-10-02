\name{SystemISCCNBS}
\alias{SystemISCCNBS}
\title{ISCC-NBS System}
\usage{SystemISCCNBS}
\description{\code{SystemISCCNBS} is a table with the ISCC-NBS System data.}
\format{
  This data frame contains the following data:
  \describe{
    \item{HueInterval}{ interval of hues in the hue chart defining the elementary block}
    \item{Hmin}{ minimum ASTM D1535 Hue for the elementary block}
    \item{Hmax}{ maximum ASTM D1535 Hue for the elementary block}
    \item{Vmin}{ minimum Value for the elementary block}
    \item{Vmax}{ maximum Value for the elementary block}
    \item{Cmin}{ minimum Chroma for the elementary block}
    \item{Cmax}{ maximum Chroma for the elementary block. Cmax=Inf for some elementary blocks.}
    \item{Number}{ color number of the elementary block, from 1 to 267}
  }
}
\references{
Color : Universal Language and Dictionary of Names
ISCC-NBS Method of Designating Colors and a Dictionary of Color Names
Kelly, Kenneth Low
Judd, Deane Brewster
NBS Special Publication 440
December 1976
Section 13: Color Name Charts, pp. 16-31
}
\author{Glenn Davis}
\examples{
data(SystemISCCNBS)
SystemISCCNBS
}
\keyword{datasets}

