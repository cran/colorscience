\name{Yuv2xy}
\alias{Yuv2xy}
\title{convert from Yuv coordinates to xy coordinates}
\description{\code{Yuv2xy} convert from Yuv coordinates to xy coordinates.
}
\usage{Yuv2xy(Yu.v.matrix) }
\arguments{
  \item{Yu.v.matrix}{ Yuv coordinates}
}
\value{
  xy coordinates
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
Yuv2xy(Yuv)
}
\keyword{datasets}

