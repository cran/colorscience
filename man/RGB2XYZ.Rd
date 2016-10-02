\name{RGB2XYZ}
\alias{RGB2XYZ}
\title{Convert sRGB coordinates to XYZ}
\description{\code{RGB2XYZ} Converts sRGB coordinates to XYZ.
}
\usage{RGB2XYZ(RGBmatrix, illuminant = "D65", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment()),
                 RGBModel = "sRGB", RefWhiteRGB = get("whitepointsRGB",
                 envir =environment()), gamma = NA,
                 RefWhiteIllum = get("XYZperfectreflectingdiffuser",
                 envir = environment()), CAT = "Bradford",
                 CATarray = get("ChromaticAdaptation", envir = environment())) }
\arguments{
  \item{RGBmatrix}{ sRGB coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ White Reference}
  \item{RGBModel}{ RGB Model}
  \item{RefWhiteRGB}{ White Reference RGB}
  \item{gamma}{ gamma}
  \item{RefWhiteIllum}{ White Reference illuminant}
  \item{CAT}{ Chromatic Adaptation algorithm}
  \item{CATarray}{ Chromatic Adaptation data}
}
\value{
  CIE XYZ coordinates
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
RGB2XYZ(c(0.310897, 0.306510, 74.613450))
}
\keyword{datasets}

