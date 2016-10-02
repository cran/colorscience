\name{makeChromaticAdaptationMatrix}
\alias{makeChromaticAdaptationMatrix}
\title{Generate a Chromatic Adaptation matrix}
\description{\code{makeChromaticAdaptationMatrix} Generates a Chromatic 
Adaptation matrix.
}
\usage{makeChromaticAdaptationMatrix(ChromaticAdaptationAlgorithm = "VonKries",
                 illuminantSource = "C", illuminantDestination = "D65",
                 observer = 2, ChromaticAdaptationArray =
                 get("ChromaticAdaptation", envir = environment()), 
  referenceWhiteArray = get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{ChromaticAdaptationAlgorithm}{ Chromatic adaptation algorithm}
  \item{illuminantSource}{ illuminant source}
  \item{illuminantDestination}{ illuminant destination}
  \item{observer}{ observer}
  \item{ChromaticAdaptationArray}{ Chromatic adaptation array}
  \item{referenceWhiteArray}{ reference white}
}
\value{
  Chromatic Adaptation matrix
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
\keyword{datasets}

