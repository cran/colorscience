\name{CIELabtoDIN99}
\alias{CIELabtoDIN99}
\title{Conversion from CIELAB color space to DIN99 coordinates}
\description{\code{CIELabtoDIN99} Converts from CIELAB color space to DIN99 
coordinates.
}
\usage{CIELabtoDIN99(Lab) }
\arguments{
  \item{Lab}{ CIELAB}
}
\value{
  DIN99
}
\source{
CIELAB to DIN99 coordinates, 2014
\url{http://de.wikipedia.org/w/index.php?title=Diskussion:DIN99-Farbraum}
}
\references{
CIELAB to DIN99 coordinates, 2014
\url{http://de.wikipedia.org/w/index.php?title=Diskussion:DIN99-Farbraum}
}
\author{Jose Gama}
\examples{
CIELabtoDIN99(c(0.310897, 0.306510, 74.613450))
}
\keyword{datasets}

