\name{rbind.AFLP.outlier}
\Rdversion{1.1}
\alias{rbind.AFLP.outlier}
\title{
Combine the information of several AFLP.outlier objects
}
\description{
Combine the information of several AFLP.outlier objects
}
\usage{
rbind.AFLP.outlier(..., deparse.level = 1)
}
\arguments{
  \item{\dots}{
Used to put two or more AFLP.outlier objects.
}
  \item{deparse.level}{
See \code{\link[base]{rbind}}
}
}
\value{
An new AFLP.outlier object combining the information of all objects.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link[base]{rbind}}
}
\examples{
  data(Tilia)
  extraOutliers <- rbind.AFLP.outlier(outliers(Tilia), outliers(Tilia))
}
\keyword{utilities}
\keyword{methods}
