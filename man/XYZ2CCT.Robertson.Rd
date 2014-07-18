\name{XYZ2CCT.Robertson}
\alias{XYZ2CCT.Robertson}
\title{convert from chromaticity coordinates to correlated color 
temperature (Robertson)}
\description{\code{XYZ2CCT.Robertson}  convert from chromaticity 
coordinates to correlated color temperature (approximation) by Robertson.
}
\usage{XYZ2CCT.Robertson(X, Y, Z) }
\arguments{
  \item{X}{ X coordinates}
  \item{Y}{ Y coordinates}
  \item{Z}{ Z coordinates}
}
\value{
  CCT (Robertson)
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
XYZ2CCT.Robertson( 0.11465380, 0.08391198, 0.08222077 )
}
\keyword{datasets}

