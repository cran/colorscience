\name{ciexyz64}
\alias{ciexyz64}
\title{CIE 1964 10-deg, XYZ CMFs}
\usage{ciexyz64}
\description{\code{ciexyz64} is a table with CIE 1964 10-deg, XYZ 
color matching functions.}
\format{
  This data frame contains the following data:
  \describe{
    \item{wlnm}{ wavelength (nm)}
    \item{xbar}{ x CMF}
    \item{ybar}{ y CMF}
    \item{zbar}{ z CMF}
  }
}
\source{
Wyszecki, G., & Stiles, W. S., 1982
Color Science: concepts and methods, quantitative data and formulae
(2nd ed.). New York: Wiley.
}
\references{
Wyszecki, G., & Stiles, W. S., 1982
Color Science: concepts and methods, quantitative data and formulae
(2nd ed.). New York: Wiley.
}
\author{Jose Gama}
\examples{
data(ciexyz64)
ciexyz64
}
\keyword{datasets}

