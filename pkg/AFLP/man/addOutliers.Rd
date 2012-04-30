\name{addOutliers}
\Rdversion{1.1}
\alias{addOutliers}
\title{
Appends outliers to an AFLP object
}
\description{
The outliers in the AFLP object are supplemented with the new outliers. 
Duplicate outliers are ignored.
}
\usage{
addOutliers(data, extraOutliers)
}
\arguments{
  \item{data}{
An AFLP object
}
  \item{extraOutliers}{
An AFLP outlier object
}
}
\value{
The \code{data} object is returned, supplemented with the outliers from \code{extraOutliers}
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{addOutliers-methods}}, \code{\link{AFLP-class}}, \code{\link{AFLP.outlier-class}}
}
\examples{
  data(Tilia)
  nOutput <- normalise(Tilia, output = "none")
  RemovingAllPossibleOutliers <- addOutliers(Tilia, nOutput$outliers)
  RemovingAllPossibleOutlieingReplicates <- 
    addOutliers(Tilia, replicates(nOutput$outliers))
}
\keyword{attribute}

