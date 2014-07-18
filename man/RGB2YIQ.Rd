\name{RGB2YIQ}
\alias{RGB2YIQ}
\title{Convert RGB coordinates to YIQ}
\description{\code{RGB2YIQ} Converts RGB coordinates to YIQ.
}
\usage{RGB2YIQ(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  YIQ coordinates
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
RGB2YIQ(RGB)
}
\keyword{datasets}

