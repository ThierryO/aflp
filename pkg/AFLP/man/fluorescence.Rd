\name{fluorescence}
\Rdversion{1.1}
\alias{fluorescence}
\title{
Extract the fluorescence (raw, normalised and scored) information from an AFLP object
}
\description{
Extract the fluorescence (raw, normalised and scored) information from an AFLP object
}
\usage{
fluorescence(x)
}
\arguments{
  \item{x}{
	An AFLP object
}
}
\value{
	The \code{Fluorescence} slot of \code{x}. 
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{AFLP-class}}
}
\examples{
#data(willow)
#fluorescence(willow)
}
\keyword{attribute}
