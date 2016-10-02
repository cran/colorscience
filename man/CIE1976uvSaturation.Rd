\name{CIE1976uvSaturation}
\alias{CIE1976uvSaturation}
\title{CIE 1976 uv Saturation}
\description{\code{CIE1976uvSaturation} CIE 1976 uv Saturation.
}
\usage{CIE1976uvSaturation(uvMatrix, whitepoint)}
\arguments{
    \item{uvMatrix}{ CIELuv data}
    \item{whitepoint}{ white point}
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
CIE1976uvSaturation(cbind(34.78467, 28.15159, 3.024663), as.numeric(
get("XYZperfectreflectingdiffuser", 
envir = environment())[which(get("XYZperfectreflectingdiffuser", 
envir = environment())[["Illuminant"]]=='C'),c('X2','Y2')]))
}
\keyword{datasets}

