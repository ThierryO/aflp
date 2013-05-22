\name{addOutliers}
\alias{addOutliers}
\title{Appends outliers to an AFLP object}
\arguments{
  \item{data}{An AFLP object}

  \item{extraOutliers}{An AFLP outlier object}
}
\value{
  The \code{data} object is returned, supplemented with the
  outliers from \code{extraOutliers}
}
\description{
  The outliers in the AFLP object are supplemented with the
  new outliers. Duplicate outliers are ignored.
}
\examples{
data(Tilia)
 nOutput <- normalise(Tilia, output = "none")
 RemovingAllPossibleOutliers <- addOutliers(Tilia, nOutput$outliers)
 RemovingAllPossibleOutlieingReplicates <-
   addOutliers(Tilia, replicates(nOutput$outliers))
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{addOutliers-methods}},
  \code{\link{AFLP-class}},
  \code{\link{AFLP.outlier-class}}
}
\keyword{attribute}

