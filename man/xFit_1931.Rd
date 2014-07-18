\name{xFit_1931}
\alias{xFit_1931}
\alias{yFit_1931}
\alias{zFit_1931}
\title{Approximations from wavelengths to XYZ by Wyman et al}
\description{\code{xFit_1931} Approximations from wavelengths
to XYZ by Wyman et al.
}
\usage{xFit_1931(wave) }
\arguments{
  \item{wave}{ wavelenght data}
}
\value{
  XYZ X, Y or Z coordinate
}
\source{
Chris Wyman Peter-Pike Sloan Peter Shirley, 2013
Simple Analytic Approximations to the CIE XYZ Color Matching Functions
Journal of Computer Graphics Techniques Vol. 2, No. 2
}
\references{
Chris Wyman Peter-Pike Sloan Peter Shirley, 2013
Simple Analytic Approximations to the CIE XYZ Color Matching Functions
Journal of Computer Graphics Techniques Vol. 2, No. 2
}
\author{Jose Gama}
\examples{
xFit_1931(555)
yFit_1931(555)
zFit_1931(555)
}
\keyword{datasets}

