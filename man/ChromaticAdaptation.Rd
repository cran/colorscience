\name{ChromaticAdaptation}
\alias{ChromaticAdaptation}
\title{Chromatic adaptation algorithms}
\usage{ChromaticAdaptation}
\description{\code{ChromaticAdaptation} chromatic adaptation algorithms 
implemented as a linear transformation (XYZ Scaling, Bradford and Von Kries).}
\format{
  This array frame contains the following dimensions:
  \describe{
    \item{1}{ rows transformation matrix 3x3}
    \item{2}{ columns transformation matrix 3x3}
    \item{3}{ linear transformation (XYZ Scaling, Bradford or Von Kries)}
    \item{4}{ transformation "direct" or "inverse"}
  }
}
\source{
Bruce Justin Lindbloom, 2013 
Color Calculator
\url{www.brucelindbloom.com}
}
\references{
Bruce Justin Lindbloom, 2013 
Color Calculator
\url{www.brucelindbloom.com}
}
\author{Jose Gama}
\examples{
data(ChromaticAdaptation)
ChromaticAdaptation
}
\keyword{datasets}

