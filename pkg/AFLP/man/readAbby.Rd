\name{readAbby}
\Rdversion{1.1}
\alias{readAbby}
\title{
Append fluorescence data from a Abby file to an AFLP object
}
\description{
Append fluorescence data from a Abby file to an AFLP object.
}
\usage{
readAbby(filename, add.to)
}
\arguments{
  \item{filename}{
	The Abby file to be imported.
}
  \item{add.to}{
	The AFLP object were the fluorescence is appended to.
}
}
\value{
	Return an AFLP object with the fluorescence data appended to it. Make sure to use the
	\code{\link{defineBins}} function prior to normalise to data.}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{defineBins}}, \code{\link{as.AFLP}}
}
\keyword{manip}
