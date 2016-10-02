\name{CIE1960UCS2CIE1964}
\alias{CIE1960UCS2CIE1964}
\title{Convert CIE 1960 UCS color space to CIE 1964 color space}
\description{\code{CIE1960UCS2CIE1964} Converts CIE 1960 UCS color space 
to CIE 1964 color space.
}
\usage{CIE1960UCS2CIE1964(uvYmatrix, illuminant = "D65", observer = 2, RefWhite =
                 get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{uvYmatrix}{ uvY data}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  CIE 1976 uv coordinates
}
\source{
Wikipedia, 2014
CIE 1964 color space
\url{http://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space}
}
\references{
Wikipedia, 2014
CIE 1931 color space
\url{http://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space}
}
\author{Jose Gama}
\examples{
CIE1960UCS2CIE1964(c(0.1633789, 1.322222, 0.08391198))
}
\keyword{datasets}

