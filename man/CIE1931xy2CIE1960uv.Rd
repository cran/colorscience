\name{CIE1931xy2CIE1960uv}
\alias{CIE1931xy2CIE1960uv}
\title{Convert CIE 1931 xy color space to CIE 1960 uv color space}
\description{\code{CIE1931xy2CIE1960uv} Converts CIE 1931 xy color space 
to CIE 1960 uv color space.
}
\usage{CIE1931xy2CIE1960uv(xymatrix) }
\arguments{
  \item{xymatrix}{ xy coordinates}
}
\value{
  CIE 1960 uv coordinates
}
\source{
Wikipedia, 2014
CIE 1931 color space
\url{http://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space}
}
\references{
Wikipedia, 2014
CIE 1931 color space
\url{http://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space}
}
\author{Jose Gama}
\examples{
xyY <- cbind(0.4083308, 0.2988462, 0.08391198)
CIE1931xy2CIE1960uv(xyY)
}
\keyword{datasets}

