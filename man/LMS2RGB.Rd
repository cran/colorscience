\name{LMS2RGB}
\alias{LMS2RGB}
\title{Convert LMS coordinates to RGB}
\description{\code{LMS2RGB} Converts LMS coordinates to RGB.
}
\usage{LMS2RGB(LMSmatrix) }
\arguments{
  \item{LMSmatrix}{ LMS coordinates}
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
LMS<-c(3.822394, 10.17498, 1.130049)
LMS2RGB(LMS)
}
\keyword{datasets}

