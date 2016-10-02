\name{Luv2Yuv}
\alias{Luv2Yuv}
\title{Convert CIE Luv coordinates to Yuv}
\description{\code{Luv2Yuv} Converts CIE Luv coordinates to Yuv.
}
\usage{Luv2Yuv(Luvmatrix,illuminant='D65',observer=2,RefWhite=
get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{Luvmatrix}{ CIE Luv coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  Yuv coordinates
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
Luv2Yuv(c(0.310897, 0.306510, 74.613450))
}
\keyword{datasets}

