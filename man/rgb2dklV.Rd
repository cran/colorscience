\name{rgb2dklV}
\alias{rgb2dklV}
\alias{rgb2dklCart}
\title{convert RGB to DKL}
\description{\code{rgb2dklV} Converts sRGB coordinates to DKL, spherical coords, 
sames as Graph-Based Visual Saliency.
\code{rgb2dklCart} Converts sRGB coordinates to DKL, cartesian coords.
}
\usage{rgb2dklV(RGB)}
\arguments{
  \item{RGB}{ sRGB coordinates}
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
rgb2dklCart(c(54,75,121))
}
\keyword{datasets}

