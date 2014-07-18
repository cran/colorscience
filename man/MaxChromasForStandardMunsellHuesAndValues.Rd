\name{MaxChromasForStandardMunsellHuesAndValues}
\alias{MaxChromasForStandardMunsellHuesAndValues}
\title{Table with maximum Munsell chroma, for a given Munsell hue and value,
for which an extrapolated renotation value is available}
\usage{MaxChromasForStandardMunsellHuesAndValues}
\description{\code{MaxChromasForStandardMunsellHuesAndValues} table with maximum 
Munsell chroma, for a given Munsell hue and value, for which an extrapolated 
renotation value is available.}
\format{
  This data frame contains the following columns:
  \describe{
    \item{H}{ Hue}
    \item{V}{ Value}
    \item{MaximumChroma}{ Maximum Chroma (MacAdam limit)}
  }
}
\source{
Paul Centore 2014
The Munsell and Kubelka-Munk Toolbox
\url{http://www.99main.com/~centore/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}

James D. Foley, Andries van Dam, Steven K. Feiner, & John F. Hughes, 1990
Computer Graphics: Principles and Practice, 2nd ed.,
Addison-Wesley Publishing Company.

Gunter Wyszecki & W. S. Stiles, 1982
Color Science: Concepts and Methods, Quantitative Data and Formulae, 2nd edition, John Wiley and Sons
}
\references{
Paul Centore 2014
The Munsell and Kubelka-Munk Toolbox
\url{http://www.99main.com/~centore/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}

James D. Foley, Andries van Dam, Steven K. Feiner, & John F. Hughes, 1990
Computer Graphics: Principles and Practice, 2nd ed.,
Addison-Wesley Publishing Company.

Gunter Wyszecki & W. S. Stiles, 1982
Color Science: Concepts and Methods, Quantitative Data and Formulae, 2nd edition, John Wiley and Sons
}
\author{Jose Gama}
\examples{
data(MaxChromasForStandardMunsellHuesAndValues)
MaxChromasForStandardMunsellHuesAndValues
}
\keyword{datasets}

