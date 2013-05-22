\name{quality<-}
\alias{quality<-}
\title{Extracts the quality data of an AFLP object}
\arguments{
  \item{x}{An AFLP object}

  \item{which}{Which quality information to extract. Must
  be one of c("all", "marker", "specimen", "replicate",
  "plate", "overall"). Defaults to "all".}
}
\value{
  In case when which == "all" a list with 3 data.frame
  containing the quality information.  Otherwise a
  data.frame with the selected quality data.
}
\description{
  Extracts the quality of an AFLP object
}
\examples{
data(Tilia)
 qc <- quality(Tilia)
 qcm <- quality(Tilia, "marker")
 qcs <- quality(Tilia, "specimen")
 qcr <- quality(Tilia, "replicate")
 qcp <- quality(Tilia, "plate")
 qcpc <- quality(Tilia, "primercombination")
 qcg <- quality(Tilia, "global")
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{quality-method}}
}
\keyword{attribute}

