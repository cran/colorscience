\name{LSLM2RGB}
\alias{LSLM2RGB}
\title{Convert LSLM coordinates to RGB}
\description{\code{LSLM2RGB} Converts LSLM coordinates to RGB.
}
\usage{LSLM2RGB(LSLMmatrix) }
\arguments{
  \item{LSLMmatrix}{ LSLM coordinates}
}
\value{
  RGB coordinates
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
LSLM<-c(-0.4186083, 0.007563981, 0.4918533)
LSLM2RGB(LSLM)
}
\keyword{datasets}

