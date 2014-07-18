\name{xyChromaticitiesVos1978}
\alias{xyChromaticitiesVos1978}
\title{x, y coordinates transformed to Judd (1951) x', y' system}
\description{\code{xyChromaticitiesVos1978} x, y coordinates transformed
to Judd (1951) x', y' system.
}
\usage{xyChromaticitiesVos1978(x,y) }
\arguments{
  \item{x}{ x coordinate}
  \item{y}{ y coordinate}
}
\value{
  x', y' coordinates
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
xyY <- c(0.4083308, 0.2988462, 0.08391198)
xyChromaticitiesVos1978(xyY[1],xyY[2])
}
\keyword{datasets}

