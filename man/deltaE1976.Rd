\name{deltaE1976}
\alias{deltaE1976}
\title{Delta E (CIE 1976)}
\description{\code{deltaE1976} The color difference Delta E (CIE 1976).
}
\usage{deltaE1976(Lab1, Lab2) }
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
deltaE1976(RGB1,RGB2)
}
\keyword{datasets}

