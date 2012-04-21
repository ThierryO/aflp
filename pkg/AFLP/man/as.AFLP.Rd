\name{as.AFLP}
\Rdversion{1.1}
\alias{as.AFLP}
\title{
Convert an object to a AFLP object
}
\description{
This function allows to convert a list of specimen or a set of positions to
an AFLP object. This is a good starting point for an analysis.
}
\usage{
as.AFLP(x, ...)
}
\arguments{
  \item{x}{
	A data.frame or a matrix containing at least the columns Plate, Capilar, Lane, Replicate 
	and Specimen. The column Group is optional.
}
  \item{\dots}{
	more arguments passed to the function.
}
}
\value{
	An AFLP object
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\examples{
  data(TiliaDesign)
  Tilia <- as.AFLP(TiliaDesign)
}
\keyword{manip}
