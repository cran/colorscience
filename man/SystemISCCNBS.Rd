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
Kelly, Kenneth Low, 1910
Central notations for the revised ISCC-NBS Color-Name Blocks 
Journal of Research of the National Bureau of Standards
Research Paper 2911, Vol. 61  No. 5, November 1958
}
\author{Glenn Davis}
\examples{
data(SystemISCCNBS)
SystemISCCNBS
}
\keyword{datasets}

