\name{dkl2dklCart}
\alias{dkl2dklCart}
\alias{dklCart2dkl}
\title{converts between spherical and cartesian coordinates for DKL}
\description{\code{dkl2dklCart} Converts DKL, from spherical coordinates 
to cartesian.
\code{dklCart2rgb} Converts DKL, from cartesian to spherical coordinates.
}
\usage{dkl2dklCart(dklMatrix)}
\arguments{
  \item{dklMatrix}{ DKL coordinates}
}
\value{
  DKL coordinates
}
\source{
Package psychopy for Python

Graph-Based Visual Saliency (MATLAB source code)
Jonathan Harel
California Institute of Technology
}
\references{
Package psychopy for Python

Graph-Based Visual Saliency (MATLAB source code)
Jonathan Harel
California Institute of Technology
}
\author{Jose Gama}
\examples{
RGB<-c(124,63,78)
d <- rgb2dklCart(RGB)
dklCart2dkl(d)
dkl2dklCart(c(1.647176, 60.8308, 91.45825))
d
}
\keyword{datasets}

