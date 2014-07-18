\name{RGB2PhotoYCC}
\alias{RGB2PhotoYCC}
\title{Convert RGB coordinates to PhotoYCC}
\description{\code{RGB2PhotoYCC} Converts RGB coordinates to PhotoYCC.
}
\usage{RGB2PhotoYCC(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  PhotoYCC coordinates
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
RGB2PhotoYCC(RGB)
}
\keyword{datasets}

