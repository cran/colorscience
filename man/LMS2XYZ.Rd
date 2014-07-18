\name{LMS2XYZ}
\alias{LMS2XYZ}
\title{Convert LMS coordinates to XYZ}
\description{\code{LMS2XYZ} Converts LMS coordinates to XYZ.
}
\usage{LMS2XYZ(LMSmatrix) }
\arguments{
  \item{LMSmatrix}{ LMS coordinates}
}
\value{
  XYZ coordinates
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
LMS2XYZ(LMS)
}
\keyword{datasets}

