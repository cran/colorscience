\name{XYZ2Yuv}
\alias{XYZ2Yuv}
\title{convert from XYZ coordinates to Yuv coordinates}
\description{\code{XYZ2Yuv} convert from XYZ coordinates to Yuv coordinates.
}
\usage{XYZ2Yuv(XYZmatrix) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
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
XYZ<-c(0.11465380, 0.08391198, 0.08222077)
XYZ2Yuv(XYZ)
}
\keyword{datasets}

