\name{YCbCr2RGB}
\alias{YCbCr2RGB}
\title{Convert YCbCr coordinates to RGB}
\description{\code{YCbCr2RGB} Converts YCbCr coordinates to RGB.
}
\usage{YCbCr2RGB(YPbPrmatrix) }
\arguments{
  \item{YPbPrmatrix}{ YPbPr coordinates}
}
\value{
  RGB coordinates
}
\source{
Alex Izvorski, Copyright 2003-2005 (Portions Copyright 2001-2003 by Alfred Reibenschuh)
Graphics/ColorObject version 0.5.0
\url{http://www.poynton.com/notes/colour_and_gamma/ColorFAQ.txt}
}
\references{
Alex Izvorski, Copyright 2003-2005 (Portions Copyright 2001-2003 by Alfred Reibenschuh)
Graphics/ColorObject version 0.5.0
\url{http://www.poynton.com/notes/colour_and_gamma/ColorFAQ.txt}
}
\author{Jose Gama}
\examples{
YCbCr2RGB(c(18165.831, -625.617, 6558.790))
}
\keyword{datasets}

