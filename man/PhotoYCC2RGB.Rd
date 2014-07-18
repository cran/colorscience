\name{PhotoYCC2RGB}
\alias{PhotoYCC2RGB}
\title{Convert PhotoYCC to RGB}
\description{\code{PhotoYCC2RGB} Converts PhotoYCC to RGB.
}
\usage{PhotoYCC2RGB(PhotoYCCmatrix) }
\arguments{
  \item{PhotoYCCmatrix}{ PhotoYCC coordinates}
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
p <- c(0.4560569, 155.9415709, 137.3026467)
PhotoYCC2RGB(p)
}
\keyword{datasets}

