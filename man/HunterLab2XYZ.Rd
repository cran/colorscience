\name{HunterLab2XYZ}
\alias{HunterLab2XYZ}
\title{Convert HunterLab coordinates to XYZ}
\description{\code{HunterLab2XYZ} Converts HunterLab coordinates to XYZ.
}
\usage{HunterLab2XYZ(HunterLabmatrix, illuminant = "D65", observer = 2,
                 RefWhite = get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{HunterLabmatrix}{ HunterLab coordinates}
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
HunterLab2XYZ(c(0.310897, 0.306510, 74.613450))
}
\keyword{datasets}

