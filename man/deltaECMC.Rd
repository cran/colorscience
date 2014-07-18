\name{deltaECMC}
\alias{deltaECMC}
\title{Delta E CMC}
\description{\code{deltaECMC} The color difference method of the Color
 Measurement Committee (the CMC) .
}
\usage{deltaECMC(Lab1, Lab2, L, C) }
\arguments{
  \item{Lab1}{ CIE Lab color sample}
  \item{Lab2}{ CIE Lab color reference}
  \item{L}{ parameter L}
  \item{C}{ parameter C}
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
deltaECMC(RGB1,RGB2)
}
\keyword{datasets}

