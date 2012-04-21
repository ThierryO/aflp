\name{AFLP.outlier}
\Rdversion{1.1}
\alias{AFLP.outlier}
\title{
Creates an AFLP outlier object
}
\description{
Creates an AFLP outlier object. The outlier object is used to mark observations as outliers in an
AFLP object. These outliers will be omitted when normalising, classifying or calculating the repeatability.
}
\usage{
AFLP.outlier(Replicate = data.frame(PC = character(), Replicate = character(), Observed = numeric()), Specimen = data.frame(PC = character(), Specimen = character(), Observed = numeric()), Marker = data.frame(PC = character(), Marker = numeric(), Observed = numeric()), Residual = data.frame(PC = character(), Replicate = character(), Marker = numeric(), Observed = numeric()))
}
\arguments{
  \item{Replicate}{
	A \code{data.frame} with three columns: PC (primer combination), Replicate and Observed.
}
  \item{Specimen}{
	A \code{data.frame} with three columns: PC (primer combination), Specimen and Observed.
}
  \item{Marker}{
	A \code{data.frame} with three columns: PC (primer combination), Marker and Observed.
}
  \item{Residual}{
	A \code{data.frame} with four columns: PC (primer combination), Replicate, Marker and Observed.
}
}
\value{
	An AFLP outlier object
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{AFLP.outlier-class}}, \code{\link{markers}}, \code{\link{replicates}}, 
\code{\link{residuals}}, \code{\link{specimens}}
}
\examples{
AFLP.outlier()
AFLP.outlier(Replicate = data.frame(PC = "PC1", Replicate = c("C.09.1744", "C.09.1745"), Observed = numeric(2)))
}
\keyword{classes}
