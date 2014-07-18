\name{Yxy2Yuv}
\alias{Yxy2Yuv}
\title{convert from Yxy coordinates to Yuv coordinates}
\description{\code{Yxy2Yuv} convert from Yxy coordinates to Yuv coordinates.
}
\usage{Yxy2Yuv(Yxymatrix) }
\arguments{
  \item{Yxymatrix}{ Yxy coordinates}
}
\value{
  Yuv coordinates
}
\source{
Bruce Justin Lindbloom, 2014
\url{http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_T.html}
}
\references{
Bruce Justin Lindbloom, 2014
\url{http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_T.html}
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
Yxy2Yuv(xyY[c(3,1,2)])
}
\keyword{datasets}

