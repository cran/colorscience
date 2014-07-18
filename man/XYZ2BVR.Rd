\name{XYZ2BVR}
\alias{XYZ2BVR}
\title{convert from XYZ coordinates to BVR }
\description{\code{XYZ2BVR}  convert from XYZ coordinates to BVR.
}
\usage{XYZ2BVR(XYZmatrix) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
}
\value{
  BVR coordinates
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
XYZ2BVR(XYZ)
}
\keyword{datasets}

