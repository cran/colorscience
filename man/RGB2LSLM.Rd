\name{RGB2LSLM}
\alias{RGB2LSLM}
\title{Convert RGB coordinates to LSLM}
\description{\code{RGB2LSLM} Converts RGB coordinates to LSLM.
}
\usage{RGB2LSLM(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  LSLM coordinates
}
\source{
Francoise Vienot, Hans Brettel,John D. Mollon, 1999
Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats
Color Research & Application
John Wiley & Sons, Inc.
}
\references{
Francoise Vienot, Hans Brettel,John D. Mollon, 1999
Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats
Color Research & Application
John Wiley & Sons, Inc.
}
\author{Jose Gama}
\examples{
RGB<-c(124,63,78)
RGB2LSLM(RGB)
}
\keyword{datasets}

