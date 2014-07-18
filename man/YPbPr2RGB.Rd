\name{YPbPr2RGB}
\alias{YPbPr2RGB}
\title{Convert YCbCr coordinates to RGB}
\description{\code{YPbPr2RGB} Converts YCbCr coordinates to RGB.
}
\usage{YPbPr2RGB(YPbPrmatrix) }
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
YPbPr2RGB(c(82.949000, -2.792896, 29.280320))
}
\keyword{datasets}

