\name{replicates}
\Rdversion{1.1}
\alias{replicates}
\title{
Extract the replicates from an object
}
\description{
	Returns the replicates from an object
}
\usage{
replicates(x)
}

\arguments{
  \item{x}{
	An AFLP or AFLP.outlier object
}
}
\value{
The replicates of the object
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{replicates-method}}, \code{\link{AFLP-class}}, \code{\link{AFLP.outlier-class}}
}
\examples{
	data(Tilia)
	replicates(Tilia)
	replicates(outliers(Tilia))
}
\keyword{attribute}

