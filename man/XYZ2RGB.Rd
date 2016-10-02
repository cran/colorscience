\name{XYZ2RGB}
\alias{XYZ2RGB}
\title{convert from XYZ coordinates to CIE RGB coordinates}
\description{\code{XYZ2RGB} convert from XYZ coordinates to CIE RGB coordinates.
}
\usage{XYZ2RGB(XYZmatrix, illuminant = "D65", observer = 2, RefWhite
                 = get("XYZperfectreflectingdiffuser", envir = environment()),
                 RGBModel = "sRGB", RefWhiteRGB = get("whitepointsRGB", 
                 envir = environment()), gamma = NA,
                 RefWhiteIllum = get("XYZperfectreflectingdiffuser",
                 envir = environment()), CAT = "Bradford",
                 CATarray = get("ChromaticAdaptation", envir = environment())) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
  \item{RGBModel}{RGB model}
  \item{RefWhiteRGB}{white points for RGB model}
  \item{gamma}{gamma}
  \item{RefWhiteIllum}{Reference perfect reflecting diffuser}
  \item{CAT}{CAT}
  \item{CATarray}{Chromatic Adaptation}
}

\value{
  CIE RGB coordinates
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
XYZ2RGB(XYZ)
}
\keyword{datasets}

