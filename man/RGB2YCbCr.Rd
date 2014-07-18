\name{RGB2YCbCr}
\alias{RGB2YCbCr}
\title{Convert RGB coordinates to YCbCr}
\description{\code{RGB2YCbCr} Converts RGB coordinates to YCbCr.
}
\usage{RGB2YCbCr(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  YCbCr coordinates
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
RGB<-c(124,63,78)
RGB2YCbCr(RGB)
}
\keyword{datasets}

