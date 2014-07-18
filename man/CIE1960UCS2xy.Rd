\name{CIE1960UCS2xy}
\alias{CIE1960UCS2xy}
\title{Convert CIE 1960 UCS color space to 1960 xy color space}
\description{\code{CIE1960UCS2xy} Converts CIE 1960 UCS color space 
to 1960 xy color space.
}
\usage{CIE1960UCS2xy(uvMatrix) }
\arguments{
  \item{uvMatrix}{ uv coordinates}
}
\value{
  CIE 1960 xy coordinates
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
CIE1960UCS2xy(c(0.1633789, 1.322222 ))
}
\keyword{datasets}

