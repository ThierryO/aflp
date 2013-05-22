\name{replicates}
\alias{replicates}
\title{Extract the replicates from an object}
\arguments{
  \item{x}{An AFLP or AFLP.outlier object}
}
\value{
  The replicates of the object
}
\description{
  Returns the replicates from an object
}
\examples{
data(Tilia)
	replicates(Tilia)
	replicates(outliers(Tilia))
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{replicates-method}},
  \code{\link{AFLP-class}},
  \code{\link{AFLP.outlier-class}}
}
\keyword{attribute}

