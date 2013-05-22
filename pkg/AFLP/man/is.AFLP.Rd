\name{is.AFLP}
\alias{is.AFLP}
\title{Checks if an object is of the AFLP class}
\usage{
  is.AFLP(x)
}
\arguments{
  \item{x}{Some object}
}
\value{
  Logical. TRUE if the object if an AFLP object. FALSE
  otherwise.
}
\description{
  Checks if an object is of the AFLP class
}
\examples{
n <- 100
 is.AFLP(n)
 data(Tilia)
 is.AFLP(Tilia)
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{AFLP-class}}, \code{\link{as.AFLP}}
}
\keyword{attribute}

