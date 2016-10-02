\name{XYZ2RxRyRz}
\alias{XYZ2RxRyRz}
\title{convert from XYZ to three filter measurements (reflectance factors)}
\description{\code{XYZ2RxRyRz} convert from XYZ to three filter measurements 
(reflectance factors).
}
\usage{XYZ2RxRyRz(XYZmatrix=NA,illuminant='C', observer=2,
RefWhite=get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{XYZmatrix}{ XYZ matrix}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ White Reference}
}
\value{
  CIE XYZ coordinates
}
\author{Jose Gama}
\examples{
XYZ<-c(0.11465380, 0.08391198, 0.08222077)
XYZ2RxRyRz(XYZ)
}
\keyword{datasets}

