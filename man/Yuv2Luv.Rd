\name{Yuv2Luv}
\alias{Yuv2Luv}
\title{Convert Yuv coordinates to Luv}
\description{\code{Yuv2Luv} Converts Yuv coordinates to Luv.
}
\usage{Yuv2Luv(Yu.v.matrix, illuminant = "D65", observer = 2,
                 RefWhite = get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{Yu.v.matrix}{ Yuv matrix}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ Reference White}
}
\value{
  Luv coordinates
}
\source{
Madk, Sophie Kirschner, 2014
Color space conversion 
\url{http://www.blitzbasic.com/codearcs/codearcs.php?code=2953}
}
\references{
Madk, Sophie Kirschner, 2014
Color space conversion 
\url{http://www.blitzbasic.com/codearcs/codearcs.php?code=2953}
}
\author{Jose Gama}
\examples{
Yuv <- c(0.08391198, 0.2830965, 0.4661789)
Yuv2Luv(Yuv)
}
\keyword{datasets}

