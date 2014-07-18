\name{HSL2RGB}
\alias{HSL2RGB}
\title{Convert HSL coordinates to RGB}
\description{\code{HSL2RGB} Converts HSL coordinates to RGB.
}
\usage{HSL2RGB(HSLmatrix) }
\arguments{
  \item{HSLmatrix}{ HSL coordinates}
}
\value{
  RGB coordinates
}
\source{
Logicol S.r.l., 2014
EasyRGB color search engine
\url{http://www.easyrgb.com/}
}
\references{
Logicol S.r.l., 2014
EasyRGB color search engine
\url{http://www.easyrgb.com/}
}
\author{Jose Gama}
\examples{
HSL<-c(0.9590164, 0.3262032, 0.3666667)
HSL2RGB(HSL)
HSL2RGB(rbind(HSL,HSL,HSL,HSL,HSL))
}
\keyword{datasets}

