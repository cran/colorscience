\name{RGB2YPbPr}
\alias{RGB2YPbPr}
\title{Convert RGB coordinates to YPbPr}
\description{\code{RGB2YPbPr} Converts RGB coordinates to YPbPr.
}
\usage{RGB2YPbPr(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  YPbPr coordinates
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
RGB2YPbPr(RGB)
}
\keyword{datasets}

