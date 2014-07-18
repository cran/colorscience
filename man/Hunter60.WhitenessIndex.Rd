\name{Hunter60.WhitenessIndex}
\alias{Hunter60.WhitenessIndex}
\title{Hunter 60 Whiteness Index}
\description{\code{Hunter60.WhitenessIndex} Hunter 60 Whiteness Index.}
\usage{Hunter60.WhitenessIndex(LabHunterMatrix)}
\arguments{
    \item{LabHunterMatrix}{ Lab Hunter values for illuminant C}
}
\source{
Xrite, 2012
Color iQC and Color iMatch Color Calculations Guide
Version 8.0
30 July 2012 Revision 1.0
}
\references{
Xrite, 2012
Color iQC and Color iMatch Color Calculations Guide
Version 8.0
30 July 2012 Revision 1.0
}
\author{Jose Gama}
\examples{
HunterLab<-c(28.96756, 2.363884, 0.4821515)
Hunter60.WhitenessIndex(HunterLab)
}
\keyword{datasets}

