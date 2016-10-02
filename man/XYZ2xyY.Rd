\name{XYZ2xyY}
\alias{XYZ2xyY}
\title{convert from XYZ coordinates to xyY coordinates}
\description{\code{XYZ2xyY} convert from XYZ coordinates to xyY coordinates.
}
\usage{XYZ2xyY(XYZmatrix,illuminant='D65',observer=2,
RefWhite=get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  xyY coordinates
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
XYZ2xyY(XYZ)
}
\keyword{datasets}

