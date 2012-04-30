\name{markers}
\Rdversion{1.1}
\alias{markers}
\title{
Extract the marker information from an object
}
\description{
Extract the marker information from an object
}
\usage{
markers(x)
}
\arguments{
  \item{x}{
An AFLP object or an AFLP.outlier object
}
}
\value{
the marker information from an object
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\examples{
  data(Tilia)
  mTilia <- markers(outliers(Tilia))
}
\keyword{attributes}
