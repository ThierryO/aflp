\name{specimens}
\alias{specimens}
\title{Extracts the specimen information from an object}
\arguments{
  \item{x}{an AFLP or AFLP.outlier object}
}
\value{
  The specimen information of the object
}
\description{
  Extracts the specimen information from an object
}
\examples{
data(Tilia)
	specimens(Tilia)
	specimens(outliers(Tilia))
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{specimens-methods}},\code{\link{AFLP-class}},\code{\link{AFLP.outlier-class}}
}
\keyword{attribute}

