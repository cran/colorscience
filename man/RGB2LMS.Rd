\name{RGB2LMS}
\alias{RGB2LMS}
\title{Convert RGB coordinates to LMS}
\description{\code{RGB2LMS} Converts RGB coordinates to LMS.
}
\usage{RGB2LMS(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  LMS coordinates
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
RGB2LMS(RGB)
}
\keyword{datasets}

