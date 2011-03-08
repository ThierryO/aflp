\name{is.AFLP}
\Rdversion{1.1}
\alias{is.AFLP}
\title{
Checks if an object is of the AFLP class
}
\description{
Checks if an object is of the AFLP class
}
\usage{
is.AFLP(x)
}
\arguments{
  \item{x}{
Some object
}
}
\value{
	Logical. TRUE if the object if an AFLP object. FALSE otherwise.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{AFLP-class}}, \code{\link{as.AFLP}}
}
\examples{
n <- 100
is.AFLP(n)
#dataset <- as.AFLP(n)
#is.AFLP(dataset)
}
\keyword{attribute}
