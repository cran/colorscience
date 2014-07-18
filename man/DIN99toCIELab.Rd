\name{DIN99toCIELab}
\alias{DIN99toCIELab}
\title{Conversion from DIN99 coordinates to CIELAB color space}
\description{\code{DIN99toCIELab} Conversion from DIN99 coordinates to 
CIELAB color space.
}
\usage{DIN99toCIELab(Lab99o) }
\arguments{
  \item{Lab99o}{ Lab99o coordinates}
}
\value{
  CIELAB coordinates
}
\source{
DIN99 coordinates to CIELAB color space
\url{http://de.wikipedia.org/w/index.php?title=Diskussion:DIN99-Farbraum}
}
\references{
DIN99 coordinates to CIELAB color space
\url{http://de.wikipedia.org/w/index.php?title=Diskussion:DIN99-Farbraum}
}
\author{Jose Gama}
\examples{
DIN99toCIELab(c(0.59072, 0.85570, 0.80283))
}
\keyword{datasets}

