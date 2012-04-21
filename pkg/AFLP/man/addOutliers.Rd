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
  data(TiliaDesign)
  Tilia <- as.AFLP(TiliaDesign)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc1", package = "AFLP"), add.to = Tilia)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc2", package = "AFLP"), add.to = Tilia)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc3", package = "AFLP"), add.to = Tilia)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc4", package = "AFLP"), add.to = Tilia)
  Tilia <- clean(Tilia)
  output <- normalise(Tilia, output = "none")
  addOutliers(Tilia, output$outliers)
  addOutliers(Tilia, replicates(output$outliers))
}
\keyword{attribute}

