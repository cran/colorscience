\name{dkl2rgb}
\alias{dkl2rgb}
\alias{dklCart2rgb}
\title{convert RGB to DKL}
\description{\code{dkl2rgb} Converts DKL, spherical coords coordinates to sRGB.
\code{dklCart2rgb} Converts DKL, cartesian coords coordinates to sRGB.
}
\usage{dkl2rgb(dklMatrix, conversionMatrix = NA)}
\arguments{
  \item{dklMatrix}{ DKL coordinates}
  \item{conversionMatrix}{ conversion matrix}
}
\value{
  RGB coordinates
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
dklC <- rgb2dklCart(c(54,75,121))
dklCart2dkl(dklC)
}
\keyword{datasets}

