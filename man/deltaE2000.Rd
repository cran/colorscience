\name{deltaE2000}
\alias{deltaE2000}
\title{Delta E (CIE 2000)}
\description{\code{deltaE2000} The color difference Delta E (CIE 2000).
}
\usage{deltaE2000(Lab1, Lab2) }
\arguments{
  \item{Lab1}{ CIE Lab color sample}
  \item{Lab2}{ CIE Lab color reference}
}
\value{
  Delta E
}
\source{
Bruce Justin Lindbloom, 2013 
Color Calculator
\url{www.brucelindbloom.com}
}
\references{
Bruce Justin Lindbloom, 2013 
Color Calculator
\url{www.brucelindbloom.com}
}
\author{Jose Gama}
\examples{
RGB1<-c(124,63,78)
RGB2<-c(95,213,184)
deltaE2000(RGB1,RGB2)
}
\keyword{datasets}

