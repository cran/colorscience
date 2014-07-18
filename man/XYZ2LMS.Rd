\name{XYZ2LMS}
\alias{XYZ2LMS}
\title{Convert XYZ coordinates to LMS}
\description{\code{XYZ2LMS} Converts XYZ coordinates to LMS.
}
\usage{XYZ2LMS(XYZmatrix) }
\arguments{
  \item{XYZmatrix}{ XYZ coordinates}
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
XYZ<-c(21.43162, -3.20673, 120.6259)
XYZ2LMS(XYZ)
}
\keyword{datasets}

