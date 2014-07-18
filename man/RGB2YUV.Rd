\name{RGB2YUV}
\alias{RGB2YUV}
\title{Convert RGB coordinates to YUV}
\description{\code{RGB2YUV} Converts RGB coordinates to YUV.
}
\usage{RGB2YUV(RGBmatrix) }
\arguments{
  \item{RGBmatrix}{ RGB coordinates}
}
\value{
  YUV coordinates
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
RGB<-c(124,63,78)
RGB2YUV(RGB)
}
\keyword{datasets}

