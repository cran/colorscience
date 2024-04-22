\name{kelvin2xy}
\alias{kelvin2xy}
\title{Blackbody radiator color temperature to CIE 1931 x,y chromaticity approximation function}
\description{\code{kelvin2xy} Blackbody radiator color temperature to CIE 1931 x,y chromaticity approximation function.
}
\usage{kelvin2xy(T) }
\arguments{
  \item{T}{ temperature in Kelvin}
}
\value{
  color temperature
}
\source{
Kim et al., 2002
"Design of Advanced Color - Temperature Control System for HDTV Applications" 
\url{https://www.jkps.or.kr/journal/view.html?uid=5163&vmd=Full}
}
\references{
Kim et al., 2002
"Design of Advanced Color - Temperature Control System for HDTV Applications" 
\url{https://www.jkps.or.kr/journal/view.html?uid=5163&vmd=Full}
}
\author{Jose Gama}
\examples{
kelvin2xy(300)
}
\keyword{datasets}

