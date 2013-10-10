\name{AFLP.outlier}
\alias{AFLP.outlier}
\title{Creates an AFLP outlier object}
\usage{
  AFLP.outlier(Replicate, Specimen, Marker, Residual)
}
\arguments{
  \item{Replicate}{A \code{data.frame} with three columns:
  PC (primer combination), Replicate and Observed.}

  \item{Specimen}{A \code{data.frame} with three columns:
  PC (primer combination), Specimen and Observed.}

  \item{Marker}{A \code{data.frame} with three columns: PC
  (primer combination), Marker and Observed.}

  \item{Residual}{A \code{data.frame} with four columns: PC
  (primer combination), Replicate, Marker and Observed.}
}
\value{
  An AFLP outlier object
}
\description{
  Creates an AFLP outlier object. The outlier object is
  used to mark observations as outliers in an AFLP object.
  These outliers will be omitted when normalising,
  classifying or calculating the repeatability.
}
\examples{
AFLP.outlier()
AFLP.outlier(
 Replicate = data.frame(
   PC = "PC1",
   Replicate = c("C.09.1744", "C.09.1745"),
   Observed = numeric(2)
 )
)
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{AFLP.outlier-class}}, \code{\link{markers}},
  \code{\link{replicates}}, \code{\link{residuals}},
  \code{\link{specimens}}
}
\keyword{classes}

