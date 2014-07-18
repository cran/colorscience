\name{deltaE1994}
\alias{deltaE1994}
\title{Delta E (CIE 1994)}
\description{\code{deltaE1994} The color difference Delta E (CIE 1994).
}
\usage{deltaE1994(Lab1, Lab2, textiles = FALSE) }
\arguments{
  \item{Lab1}{ CIE Lab color sample}
  \item{Lab2}{ CIE Lab color reference}
  \item{textiles}{ boolean, TRUE = version for textiles}
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
deltaE1994(RGB1,RGB2)
}
\keyword{datasets}

