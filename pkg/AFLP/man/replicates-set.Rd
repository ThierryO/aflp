\name{replicates<-}
\alias{replicates<-}
\title{Adds or overwrites the replicates}
\arguments{
  \item{data}{An AFLP object}

  \item{value}{A data.frame with the replicate information}
}
\value{
  \code{data} is updated with the new replicate information
}
\description{
  Adds or overwrites the replicates in an AFLP object.
}
\examples{
data(Tilia)
 tmp <- replicates(Tilia)
 tmp$Plate[1] <- "8"
 replicates(Tilia) <- tmp
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{replicate}}
}
\keyword{manip}

