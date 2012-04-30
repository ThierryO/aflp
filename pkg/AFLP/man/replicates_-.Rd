\name{replicates<-}
\Rdversion{1.1}
\alias{replicates<-}
\title{
Adds or overwrites the replicates
}
\description{
Adds or overwrites the replicates in an AFLP object.
}
\usage{
replicates(data) <- value
}
\arguments{
  \item{data}{
	An AFLP object
}
  \item{value}{
	A data.frame with the replicate information
}
}
\value{
	\code{data} is updated with the new replicate information
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{replicate}}
}
\examples{
  data(Tilia)
  tmp <- replicates(Tilia)
  tmp$Plate[1] <- "8"
  replicates(Tilia) <- tmp
}
\keyword{manip}
