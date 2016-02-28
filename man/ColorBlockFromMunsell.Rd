\name{ColorBlockFromMunsell}
\alias{ColorBlockFromMunsell}
\title{convert Munsell notation to numeric HVC}
\description{\code{ColorBlockFromMunsell} Converts convert Munsell notation 
to numeric Hue,Value,Chroma.
}
\usage{ColorBlockFromMunsell(HVC) }
\arguments{
\item{HVC}{ Munsell hue, value and chroma }
}
\format{
  \describe{
  \item{HVC[1]}{ Munsell hue, on the ASTM D1535 100 point circular scale. All values are valid.}
  \item{HVC[2]}{ Munsell value, must be between 0 and 10}
  \item{HVC[3]}{ Munsell chroma, must be non-negative}
}
}
\value{
  HVC, ISCC-NBS Number, ISCC-NBS Name
}
\author{Glenn Davis}
\keyword{datasets}

