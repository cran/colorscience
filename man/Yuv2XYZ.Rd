\name{Yuv2XYZ}
\alias{Yuv2XYZ}
\title{convert from Yuv coordinates to XYZ coordinates}
\description{\code{Yuv2XYZ} convert from Yuv coordinates to XYZ coordinates.
}
\usage{Yuv2XYZ(Yu.v.matrix) }
\arguments{
  \item{Yu.v.matrix}{ Yuv coordinates}
}
\value{
  XYZ coordinates
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
Yuv <- c(0.08391198, 0.2830965, 0.4661789)
Yuv2XYZ(Yuv)
}
\keyword{datasets}

