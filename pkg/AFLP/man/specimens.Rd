\name{specimens}
\Rdversion{1.1}
\alias{specimens}
\title{
Extracts the specimen information from an object
}
\description{
Extracts the specimen information from an object
}
\usage{
specimens(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an AFLP or AFLP.outlier object
}
}
\value{
	The specimen information of the object
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{specimens-methods}},\code{\link{AFLP-class}},\code{\link{AFLP.outlier-class}}
}
\examples{
	#data(willow)
	#specimens(willow)
	#data(willowN)
	#specimens(outliers(willowN))
}
\keyword{attribute}
