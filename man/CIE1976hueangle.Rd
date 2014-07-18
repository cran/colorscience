\name{CIE1976hueangle}
\alias{CIE1976hueangle}
\title{CIE 1976 hue angle formula for CIELab and CIELuv}
\description{\code{CIE1976hueangle} CIE 1976 hue angle formula for CIELab and CIELuv.
}
\usage{CIE1976hueangle(CIELMatrix)}
\arguments{
    \item{CIELMatrix}{ CIELab or CIELuv data}
}
\source{
R. W. G. Hunt, M. R. Pointer, 2011
Measuring Colour
Volume 23 of The Wiley-IS&T Series in Imaging Science and Technology
John Wiley & Sons
}
\references{
R. W. G. Hunt, M. R. Pointer, 2011
Measuring Colour
Volume 23 of The Wiley-IS&T Series in Imaging Science and Technology
John Wiley & Sons
}
\author{Jose Gama}
\examples{
CIELMatrix<-c(34.78467, 28.15159, 3.024663)
CIE1976hueangle(CIELMatrix)
}
\keyword{datasets}

