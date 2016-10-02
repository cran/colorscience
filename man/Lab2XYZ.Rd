\name{Lab2XYZ}
\alias{Lab2XYZ}
\title{Convert CIE Lab coordinates to XYZ}
\description{\code{Lab2XYZ} Converts CIE Lab coordinates to XYZ.
}
\usage{Lab2XYZ(Labmatrix, illuminant = "D65", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{Labmatrix}{ CIE Lab coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  XYZ coordinates
}
\source{
Logicol S.r.l., 2014
EasyRGB color search engine
\url{http://www.easyrgb.com/}
}
\references{
Logicol S.r.l., 2014
EasyRGB color search engine
\url{http://www.easyrgb.com/}
}
\author{Jose Gama}
\examples{
Lab2XYZ(c(0.310897, 0.306510, 74.613450))
}
\keyword{datasets}

