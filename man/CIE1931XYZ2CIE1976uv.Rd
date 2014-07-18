\name{CIE1931XYZ2CIE1976uv}
\alias{CIE1931XYZ2CIE1976uv}
\title{Convert CIE 1931 XYZ color space to CIE 1976 uv color space}
\description{\code{CIE1931XYZ2CIE1976uv} Converts CIE 1931 XYZ color space 
to CIE 1976 uv color space.
}
\usage{CIE1931XYZ2CIE1976uv(XYZmatrix) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
}
\value{
  CIE 1976 uv coordinates
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
XYZ<-c(0.11465380, 0.08391198, 0.08222077)
CIE1931XYZ2CIE1976uv(XYZ)
}
\keyword{datasets}

