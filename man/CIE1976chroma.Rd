\name{CIE1976chroma}
\alias{CIE1976chroma}
\title{CIE 1976 chroma formula for CIELab and CIELuv}
\description{\code{CIE1976chroma} CIE 1976 chroma formula for CIELab and CIELuv.
}
\usage{CIE1976chroma(CIELMatrix)}
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
CIE1976chroma(CIELMatrix)
}
\keyword{datasets}

