\name{quality}
\Rdversion{1.1}
\alias{quality}
\title{
Extracts the quality data of an AFLP object
}
\description{
Extracts the quality of an AFLP object
}
\usage{
quality(x, which = c("all", "marker", "specimen", "overall"))
}
\arguments{
  \item{x}{
	An AFLP object
}
  \item{which}{
	Which quality information to extract. Must be one of c("all", "marker", "specimen", 
	"overall"). Defaults to "all".
}
}
\value{
	In case when which == "all" a list with 3 data.frame containing the quality information.
	Otherwise a data.frame with the selected quality data.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}

\seealso{
\code{\link{quality-method}}
}
\examples{
	#data(willowN)
	#border(willowN)
}
\keyword{attribute}
