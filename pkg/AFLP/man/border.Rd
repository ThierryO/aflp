\name{border}
\Rdversion{1.1}
\alias{border}
\title{
Extracts the cut-levels of an AFLP object
}
\description{
Extracts the cut-levels of an AFLP object
}
\usage{
border(data, pc, marker)
}
\arguments{
  \item{data}{
	An AFLP object
}
  \item{pc}{
	The primer combination for which we want the cut-levels.
}
  \item{marker}{
	The marker for which we want the cut-levels.
}
}
\value{
	A vector of cut-levels.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}

\seealso{
\code{\link{border-method}}
}
\examples{
  data(Tilia)
  output <- normalise(Tilia, output = "none")$data
  output <- classify(output, output = "none")
  BorderList <- border(output)
}
\keyword{attribute}
