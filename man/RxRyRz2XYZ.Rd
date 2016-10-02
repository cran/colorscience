\name{RxRyRz2XYZ}
\alias{RxRyRz2XYZ}
\title{convert from three filter measurements (reflectance factors) to XYZ}
\description{\code{RxRyRz2XYZ} convert from three filter measurements 
(reflectance factors) to XYZ.
}
\usage{RxRyRz2XYZ(RxRyRzmatrix=NA,illuminant='C', observer=2,
RefWhite=get("XYZperfectreflectingdiffuser", envir = environment())) }
\arguments{
  \item{RxRyRzmatrix}{ reflectance factors coordinates}
  \item{illuminant}{ illuminant}
  \item{observer}{ observer}
  \item{RefWhite}{ White Reference}
}
\value{
  CIE XYZ coordinates
}
\author{Jose Gama}
\examples{
RxRyRz2XYZ(c(7.90393, 8.391198, 9.721126))
}
\keyword{datasets}

