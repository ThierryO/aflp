\name{randomiseSlabgel}
\Rdversion{1.1}
\alias{randomiseSlabgel}
\title{
Randomise specimens over different slab gels.
}
\description{
This function randomises Specimens over the required number of plates depending on the size of the
plates, the minimum number of replicated specimens per plate and the number of quality control 
specimens per plate. 

The function ensures that at least \code{nReplicates} replications within  each plate. These 
specimens are also replicated on one of the other plates. This has two advantages. First it helps
to quantify the plate effect in the normalisation. Second it generates the replication needed to
evaluate the repeatability.

Any left-over lanes on a plate are filled with additional replications of specimens which have yet
been replicated.
}
\usage{
randomiseSlabgel(Specimens, Group = NULL, nReplicates = 3, nQC = 0, nLanes = 64)
}
\arguments{
  \item{Specimens}{
	Either the number of specimens or a vector with the names of the specimens.
}
  \item{Group}{
  	A vector indication the a priori clustering of specimens. Must be as long as the number of
  	specimens and in the same order. When missing, no a priori clustering is assumed. Defaults 
  	to NULL.
}
  \item{nReplicates}{
  	The minimum number specimens that will be replicated on the same gel.
}
  \item{nQC}{
	The number of quality control specimens. They are appended to the Specimens. There position
	is fixed at the last positions of each plate.
}
  \item{nLanes}{
	The number of lanes on a plate.
}
}
\value{
	Results in an AFLP object with randomised replicates.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{as.AFLP}}, \code{\link{normalise}}
}
\examples{
	randomiseSlabgel(10)
	randomiseSlabgel(150, Group = gl(3, 50), nQC = 2)
}
\keyword{design}
