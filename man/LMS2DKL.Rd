\name{LMS2DKL}
\alias{LMS2DKL}
\title{Convert LMS coordinates to DKL}
\description{\code{LMS2DKL} Converts LMS coordinates to DKL.
}
\usage{LMS2DKL(bg, diffcone.coords, DKL2LMS = FALSE) }
\arguments{
  \item{bg}{ LMS coordinates}
  \item{diffcone.coords}{ LMS coordinates}
  \item{DKL2LMS}{ boolean, FALSE = DKL to LMS, TRUE = LMS to DKL}
}
\value{
  DKL coordinates
}
\source{
David H. Brainard
Cone Contrast and Opponent Modulation Color Spaces
pp. 563
PART IV: CONE CONTRAST AND OPPONENT MODULATION COLOR SPACES
}
\references{
David H. Brainard
Cone Contrast and Opponent Modulation Color Spaces
pp. 563
PART IV: CONE CONTRAST AND OPPONENT MODULATION COLOR SPACES
}
\author{Jose Gama}
\examples{
#LMS<-c(3.822394, 10.17498, 1.130049)
#LMS2DKL(LMS)
}
\keyword{datasets}

