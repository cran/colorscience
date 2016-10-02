\name{XYZ2Luv}
\alias{XYZ2Luv}
\title{convert from XYZ coordinates to CIE Luv coordinates}
\description{\code{XYZ2Luv} convert from XYZ coordinates to CIE Luv coordinates.
}
\usage{XYZ2Luv(XYZmatrix,illuminant='D65',observer=2,
RefWhite=get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  CIE Luv coordinates
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
CIELuvMatrix<-xyz2luv(XYZ*100)
}
\keyword{datasets}

