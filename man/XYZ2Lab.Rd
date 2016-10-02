\name{XYZ2Lab}
\alias{XYZ2Lab}
\title{convert from XYZ coordinates to CIE Lab coordinates}
\description{\code{XYZ2Lab} convert from XYZ coordinates to CIE Lab coordinates.
}
\usage{XYZ2Lab(XYZmatrix,illuminant='D65',observer=2, 
    RefWhite=get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  CIE Lab coordinates
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
CIELMatrix<-xyz2lab(XYZ*100)
}
\keyword{datasets}

