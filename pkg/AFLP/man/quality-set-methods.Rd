\name{quality<--methods}
\alias{quality<--methods}
\alias{quality<-,AFLP-method}
\title{Adds or overwrites quality data}
\arguments{
  \item{data}{An AFLP object}

  \item{value}{When which == "all", value must be a list
  with quality information. Otherwise a data.frame with the
  corresponding quality information.}

  \item{which}{A character value matching c("all",
  "marker", "specimen", "replicate", "plate",
  "primercombinations", "global"). Defaults to "all".}
}
\value{
  \code{data} is updated with the new quality information
}
\description{
  Adds or overwrites quality in an AFLP object.
}
\examples{
data(Tilia)
 qc <- quality(Tilia)
 qc$Global$Score <- NA
 quality(Tilia) <- qc
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{quality}}
}
\keyword{manip}

