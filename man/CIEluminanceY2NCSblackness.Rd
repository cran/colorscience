\name{CIEluminanceY2NCSblackness}
\alias{CIEluminanceY2NCSblackness}
\title{approximated NCS blackness s by the CIE luminance factor Y}
\description{\code{CIEluminanceY2NCSblackness} approximated NCS blackness s 
by the CIE luminance factor Y.
}
\usage{CIEluminanceY2NCSblackness(Y)}
\arguments{
    \item{Y}{ CIE values for illuminant C}
}
\source{
Hsien-Che Lee, 2005
Introduction to Color Imaging Science
Cambridge University Press
pp. 366
}
\references{
Hsien-Che Lee, 2005
Introduction to Color Imaging Science
Cambridge University Press
pp. 366
}
\author{Jose Gama}
\examples{
xyY <- c(0.4083308, 0.2988462, 0.08391198)
CIEluminanceY2NCSblackness(xyY[3])
}
\keyword{datasets}

