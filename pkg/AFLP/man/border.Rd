\name{border}
\alias{border}
\title{Extracts the cut-levels of an AFLP object}
\arguments{
  \item{data}{An AFLP object}

  \item{pc}{The primer combination for which we want the
  cut-levels.}

  \item{marker}{The marker for which we want the
  cut-levels.}
}
\value{
  A vector of cut-levels.
}
\description{
  Extracts the cut-levels of an AFLP object
}
\examples{
data(Tilia)
 BorderList <- border(Tilia)
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{border-method}}
}
\keyword{attribute}

