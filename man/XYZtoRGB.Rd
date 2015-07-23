\name{XYZtoRGB}
\alias{XYZtoRGB}
\title{convert from XYZ coordinates to RGB}
\description{\code{XYZtoRGB} convert from XYZ coordinates to RGB.
}
\usage{XYZtoRGB(xc, yc, zc, ColorSystem = c(0.67, 0.33, 0.21, 0.71,
                 0.14, 0.08, 0.31, 0.316)) }
\arguments{
  \item{xc}{ XYZ X coordinates}
  \item{yc}{ XYZ Y coordinates}
  \item{zc}{ XYZ Z coordinates}
  \item{ColorSystem}{ RGB Color System data}
}
\value{
  RGB coordinates
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
XYZtoRGB(0.11465380, 0.08391198, 0.08222077)
}
\keyword{datasets}

