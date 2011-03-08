\name{QC}
\Rdversion{1.1}
\alias{QC}
\title{
Extracts the quality control samples of an AFLP object
}
\description{
Extracts the quality control samples of an AFLP object
}
\usage{
QC(data, which = c("all", "specimen", "replicate"))
}
\arguments{
  \item{data}{
	An AFLP object
}
  \item{which}{
	Which quality information to extract. Must be one of c("all", "specimen", 
	"replicate"). Defaults to "all".
}
}
\value{
	In case when which == "all" a list with 2 data.frames containing the quality control samples.
	Otherwise a data.frame with the selected quality control samples.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}

\seealso{
\code{\link{QC-method}}
}
\examples{
	#data(willowN)
	#border(willowN)
}
\keyword{attribute}
