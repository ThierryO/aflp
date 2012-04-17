\name{readSAGA}
\Rdversion{1.1}
\alias{readSAGA}
\title{
Append fluorescence data from a SAGA file to an AFLP object
}
\description{
Append fluorescence data from a SAGA file to an AFLP object.

NOTE: the SAGA file must be in the default CSV format.
}
\usage{
readSAGA(filename, add.to, maxMissing = 0.25, textclean = function(x){x})
}
\arguments{
  \item{filename}{
	The SAGA file to be imported.
}
  \item{add.to}{
	The AFLP object were the fluorescence is appended to.
}
  \item{maxMissing}{
	A relative threshold. If more than this ratio of replicates have missing data, then
	no data is appended.
}
  \item{textclean}{
	A user defined function to alter the names of the replicates after reading the SAGA file.
	Only useful when the names in the SAGA files do not match with the names in the AFLP
	object. Directly using the correct names is SAGA solves a lot of problems.
}
}
\value{
	Return an AFLP object. If add.to is an AFLP object, then the fluorescence data
	is appended to it. Otherwise a new AFLP object is created.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{as.AFLP}}
}
\examples{
data(TiliaDesign)
Tilia <- as.AFLP(TiliaDesign)
Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc1", package = "AFLP"), add.to = Tilia)
Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc2", package = "AFLP"), add.to = Tilia)
Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc3", package = "AFLP"), add.to = Tilia)
Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc4", package = "AFLP"), add.to = Tilia)
}
\keyword{manip}
