\name{compuphaseDifferenceRGB}
\alias{compuphaseDifferenceRGB}
\title{compuphase Difference RGB}
\description{\code{compuphaseDifferenceRGB} compuphase Difference RGB.
}
\usage{compuphaseDifferenceRGB(RGB1, RGB2) }
\arguments{
  \item{RGB1}{ RGB color sample}
  \item{RGB2}{ RGB color reference}
}
\value{
  Delta E
}
\source{
Thiadmer Riemersma, 2012
CompuPhase
\url{http://www.compuphase.com/cmetric.htm}
}
\references{
Thiadmer Riemersma, 2012
CompuPhase
\url{http://www.compuphase.com/cmetric.htm}
}
\author{Jose Gama}
\examples{
compuphaseDifferenceRGB(c(124,63,78),c(241,65,78))
}
\keyword{datasets}

