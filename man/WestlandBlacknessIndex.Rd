\name{WestlandBlacknessIndex}
\alias{WestlandBlacknessIndex}
\title{ Westland, et al. blackness index}
\description{\code{WestlandBlacknessIndex} (Westland, et al., 2006) blackness 
index.
}
\usage{WestlandBlacknessIndex(CIELabMatrix) }
\arguments{
  \item{CIELabMatrix}{ CIELab coordinates}
}
\value{
  blackness index
}
\source{
Westland, S.; Cheung, T. L. V.; Lozman, O. R., 2006.
A metric for predicting perceptual blackness.
14th Color Imaging Conference Final Program and Proceedings, 14-17.}
\references{
Westland, S.; Cheung, T. L. V.; Lozman, O. R., 2006.
A metric for predicting perceptual blackness.
14th Color Imaging Conference Final Program and Proceedings, 14-17.
}
\author{Jose Gama}
\examples{
CIELab<-c(34.78467, 28.15159, 3.024663)
WestlandBlacknessIndex(CIELab)
}
\keyword{datasets}

