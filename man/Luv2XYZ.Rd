\name{Luv2XYZ}
\alias{Luv2XYZ}
\title{Convert CIE Luv coordinates to XYZ}
\description{\code{Luv2XYZ} Converts CIE Luv coordinates to XYZ.
}
\usage{Luv2XYZ(Luvmatrix, illuminant = "D65", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{Luvmatrix}{ Luv matrix}
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
Luv2XYZ(c(0.310897, 0.306510, 74.613450))
}
\keyword{datasets}

